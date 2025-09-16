+++
title = "Talk:RSA code"
description = ""
date = 2011-05-23T13:36:27Z
aliases = []
[extra]
id = 9385
[taxonomies]
categories = []
tags = []
+++

== Difference from practical cryptographical version ==

The are a number of differences between the bare algorithm presented on this page and what you'll see in real use.
* There's nothing to prevent tampering (such as including the length and a cryptographic hash in the encoded message).
* The keys are significantly smaller than in real deployments (where keys are often several kilobits long; this great length by comparison with symmetric cyphers – e.g., Blowfish – is required because of the relative sparseness of prime numbers, which are vital for building the keys).
** Absolutely true, the updated task only required using keys that exercised large integers and could potentially handle real keys.  Even the long demo key is considerably shorter than RSA 129 (43 vs 129) --[[User:Dgamey|Dgamey]] 13:30, 23 May 2011 (UTC)
* Solutions here are not required to handle putting the message into blocks if it is too long (i.e., if the values are larger than the modulus part of the key).
** The updated description requires that this be done or the limitation called out.  --[[User:Dgamey|Dgamey]] 13:30, 23 May 2011 (UTC)
* Solutions here are not required to compress the data before encrypting it (vital in general because it helps to reduce the amount of redundant information in the encrypted message and so frustrate cryptanalysis).
** Excellent point!  --[[User:Dgamey|Dgamey]] 13:30, 23 May 2011 (UTC)
'''''Do not use these solutions in deployed code''''', or at least not without consulting both a cryptographer and an experienced software engineer! (Also, be aware that the generation of RSA keys is an entirely more complex problem than applying the encryption and decryption algorithm.) –[[User:Dkf|Donal Fellows]] 09:28, 23 May 2011 (UTC)

==Draft==
There's a ''lot'' to do to clean this up to the level of being a full task. For one thing, it's not at all clear what this code is trying to do! It also lacks links to resources (e.g., wikipedia) that describe the algorithm involved, and the Python solution needs much work too (especially in its surrounding descriptive text, which looks to me more like text that belongs here on the talk page). –[[User:Dkf|Donal Fellows]] 06:31, 24 March 2011 (UTC)

:+1 on Dkf's comments. How to perform the task needs to be in the task description in a more language neutral form.  --[[User:Paddy3118|Paddy3118]] 13:12, 24 March 2011 (UTC)

:Sorry about the shoddy state of my code, it's one of the first programs i have written on my own, and there are probably far more efficient ways of performing many functions that i just brute forced my way past. But it works, and i am rather proud of it, ugly as it may be. I have added detail about the function of the code and the way the RSA algorithm works. Please let me know if there is anything else i can clear up! --[[User:Erasmus|Erasmus]] 03:00, 26 March 2011 (UTC)

:I believe my code is now annotated enough so that it makes sense to read it. I am streamlining (at least as much as i can) a program to generate new keys to use, i will upload that when i am done --[[User:Erasmus|Erasmus]] 02:21, 29 March 2011 (UTC)

:: OK, things are starting to look better. We still lack a description of what the task ''is'' though! I guess it's to do the encryption and decryption (with fixed trivial keys?) and not to do key generation; the latter is much more computationally expensive and doesn't serve as an introduction to this area of cryptography. –[[User:Dkf|Donal Fellows]] 07:26, 2 April 2011 (UTC)


### Updated Task Description

Nobody had taken it upon themselves to update and clarify the task after last months discussions.  That and no one was adding code.  So I  jumped in.

As it currently stands the task is very flexible.  You must implement RSA and be able to encrypt and decrypt messages, support character to number encoding and decoding. Support (or warn about) blocking. Demonstrate you can support real key lengths by using large integers. And demonstrated it works by showing intermediate results.  You are free to pick your keys and encoding scheme although examples have been provided.  For the examples added prior to GO the only hardship here might be the need to use large integers. Unfortunately one example just calls an RSA library.  As that example was added later and there are lots of tasks about calling libraries, I believe that example should be marked incorrect.

I'd like to request that the Python author remove the massive UI code.  Park it here in a section if you like and reference it.

Some of the other suggestions (including mine for RSA129) would have invalidated most of the original contributions.  I hope this change strikes a reasonable compromise between respecting the work of the original contributors and strengthening the task.

I believe that this is now ready to graduate out of draft.  I propose to let it sit a few weeks to give people a chance to respond before removing draft.

Thanks  --[[User:Dgamey|Dgamey]] 03:21, 18 May 2011 (UTC)


### Solutions are starting to diverge


As I see it the specification should be tightened up.  People are starting to add solutions that are visibly different.  Go for example.  The solution is a good solution and answers some of the problems but the task description doesn't cover it.   It's significantly different from some of the others.  It uses a larger n,e,d and doesn't need blocking for short messages.  

Without some consensus we could easily end up all over the map with 3 or 4 close tasks.

--[[User:Dgamey|Dgamey]] 02:49, 27 April 2011 (UTC)

### Library or Implementation? The direction of PicoLisp

Again we need to settle on a definition before too many people add solutions.  Just calling a library may be a valid task but it doesn't fit the original task description or the alternate started in GO.  A similar debate was held in [[MD5]] and [[MD5/Implementation]]. --[[User:Dgamey|Dgamey]] 10:15, 29 April 2011 (UTC)

:* I think a warning on the main page is due.  --[[User:Dgamey|Dgamey]] 10:15, 29 April 2011 (UTC)


### The direction of GO

: Deliberately so. :)  With the task in draft status I thought I would mix things up with some different ideas, and thought I would start with some code as an illustration.  So in no particular order here, I was thinking a number of things.
:* The task has potential to get uncomfortably large and multi-faceted.  I wanted to see if I could write something compact.  (Well, as Go code goes.  I know Go tends to run long in LOC.)
:* I think big numbers are an important part of RSA, but when I tried RSA-129, it looked a little messy, requiring either literals split across multiple lines or else lines much longer than a punch card.
:* Real encryption involves a number of details in the blocking.  There's not only the splitting, but there's usually a few steps of padding and hashing too.  This seems a distraction from the interesting number-theoretic algorithm.  Still, the task as worded wanted text-to-text encryption to decryption, so I tried a minimal way of doing that, just laying out bytes and not even mapping the character set.  (It could even be done more directly but I wanted to make my technique as obvious as possible.)
:* Also I don't bother converting the encrypted number to something transmittable like a printable string or byte array.  Is it enough to show the encrypted number?  Should the task require sequencing blocks into a byte stream?
:* Encryption and decryption are symmetric--it's the exact same function.  I thought I would write a common function that I would call for both purposes.  Then I thought, that's silly, why repeat?  Maybe the task should just give an encrypted message and ask to decode and print it.  Then I realized there's nothing to write--the exact function, the exp mod thing, is in the Go library.  But then, small values of E are the norm.  An encryption function might be written to run faster by taking a small integer type as E rather than a big integer type.  But then, RC tasks aren't generally about optimization...
:* Subtasks may well be in order.  I think key generation would be a good subtask.  I found it was non-trivial to generate one just the way I wanted it.  (I'm still not sure it's right, btw.)  Blocking might be another good subtask.  Then the task writer might feel free to require several steps of padding and hashing and so on.  The exp mod function could be a subtask, but a) it's pretty simple, and b) I know I've written it for at least one RC task already, Factors of a Mersenne, and I'm not sure, I kind of remember it somewhere else too.  I don't know, maybe that's more of a reason for it to be a separate task, so it could be referenced from these other places as well.
:* I barely know what I'm talking about.  What is this PKCS#1 thing?  Do we want to go there?
:* These are just ideas to toss around.  I'll happily change my code to match whatever the task settles on.
: &mdash;[[User:Sonia|Sonia]] 05:22, 27 April 2011 (UTC)

:: Now this is a good discussion about drafts :)

::* I like the size of the number.  Big enough to test bignums and small enough to fit within the margins.  BTW if you try just plain bignum m^e%n your bignums may choke even with this one.
::* Where did you get you numbers from BTW?
:::: Go has an RSA package that will generate a key of any bit length.  I had to modify the code to use 65537 for E, which I thought would be appropriate for our use.  &mdash;[[User:Sonia|Sonia]] 02:44, 28 April 2011 (UTC)
::* You just choose ASCII encoding - the new/modified task would need to say so.
::* Any encryption task will need a disclaimer on the page (demonstration numbers, key to short, lack of random padding, not to mention subtle attacks if e is chosen improperly.
::* Blocking could be done in a another task.  It's needed if m > n.  And in the particular cases of the original task where the character encoding is sparse 30+ chars into 100s the decoding will blow up.  With ASCII encoding it doesn't blow up but you get gibberish. 
:::''In the general case, block size for encoded text, represented as text, will need to be at least 1 character larger than the unencrypted text.  And 1 character is only possible when the block size was picked to be the largest possible for N. (And there is also the issue of determining the original message length -- humans can maybe ignore padding but our programs are easily confused)'' --[[User:Rdm|Rdm]] 14:00, 27 April 2011 (UTC)
:::: I tried a couple of ways.  First blocking before encoding and working back the blocking size to the text which I found to be messy. Later and easier just use the power of 2 just less than m for blocking.  However, at some point I'll probably blow up something in the bignum support.  Using decode(1 + size of n) would give it in characters just as easily. --[[User:Dgamey|Dgamey]] 03:27, 28 April 2011 (UTC)
::* An RSA/keygen task would be another good task.  There are even subtasks possible within that.
::* A general character encoding and blocking task would be good
::* An RSA/129 task might be good.  But the 80 column limit would have to be forgiven OR use files and a formats output to "12345 .... 9999" with dots to some fixed width
::* Not overly interested in doing PKCS#n too.
::* It's useful to see the interim products.  I liked that.
::* It may be useful to show the concept of signing, decrypting a message, than anyone can encrypt to get the plain text.  But that was part of RSA 129 challenge so maybe better left to that task.  Again, real signing is more complex.
::* How about (in summary) the new/modified task is something like this:
:::  To Encrypt and Decrypt a short message or two using RSA with a demonstration key (we should all use the same one)
:::  Simple ASCII encoding and decoding of the message
:::  Give an error if m > n (i.e. would require blocking)
:::  Exercise large integer support or libraries (no need to include, just reference unless they were purpose written for this task)
:::  Show intermediate results
:::  No UI - it should be a separate non-crypto tasks, hard code or simple input
:::  Background on RSA
:::  Dislaimer about real crypto
:::  Bonus points for handling blocking 

::* I'll post some code that follows your example.  But it won't be until at least this evening.

::: Oh, I ''like'' this version very much; the Go code is clear and it was enough for me to figure out exactly what I needed to do (along with the formulæ already in the task description). All I needed was a modexp implementation, which wasn't hard to find. I modified things slightly in the Tcl solution to force UTF-8 (on the grounds that it's trivial for me to do it and it gets rid of one of the restrictions mentioned above). What I considered, but didn't do in the end, was adding blocking to allow arbitrary length messages. I'm not sure that there's a huge need to show intermediate results though; the interpretation of the messages as numbers is not really very interesting after all and it makes a functional encryption interface much messier. –[[User:Dkf|Donal Fellows]] 09:09, 23 May 2011 (UTC)


### = New/Modified Tasks and Categories =

:: How do we get a consensus on this?  I don't like stealing a task and would like to hear from the Author.  Would we create a new task and put something very visible up front in this saying something about it?  Would we rename this to RSAdraftprototype or some such? Do we copy to new and mark the others incorrect? Erasmus?
:: --[[User:Dgamey|Dgamey]] 11:04, 27 April 2011 (UTC) 

:::I think the first thing to do is create a category (or maybe categories?) for these tasks.  Then mark this task with that category.  Then do up some draft task pages and see if they attract interest.  I think that this one can be cleaned up, later, perhaps after we have had some practice with formulating tasks related to encryption, or perhaps by the original author.  --[[User:Rdm|Rdm]] 14:00, 27 April 2011 (UTC)

:::: Ok.  There is a [[:Category:Encryption|Encryption Category]] containing just ROT-13.  There is also MD5 while although hashing is based on cryptography.  There are also related pages like Knapsack problems.  I can think of all kinds of possible and interesting subgroups (not all of which are mutually exclusive).  Classical, Machine, Modern, Symmetric Key, Public Key, Hashing, Cryptanalysis, ....  I don't want to go overboard and I have no experience with organizing categories on RC.  I don't know what works.  I'm thinking two layers with one or two major containers like 'Cryptography' and everything else held with it (by convention 'Cryptography' and 'Public Key' or by combined name like 'Cryptography/Public Key').  A task could have multiple labels.  Also if you look at MD5 there is MD5 (call a library) or MD5/Implementation.  Without some structure it could get messy.  Any thoughts? --[[User:Dgamey|Dgamey]] 03:17, 28 April 2011 (UTC)

::::: Thoughts.... simplicity is a virtue?  Here, let's limit ourselves to just one category for now, based on the variant tasks we would want to tackle first.  We can always add categories later.  We could even go ahead and draft up some new tasks first and hold off on picking categories for now. 

::::: In other words, I am going to downplay my earlier suggestion and suggest instead that we just come up with some simple tasks that seem to be good ones.  Similarly, I would not worry too much about stealing a task:  we have wiki edit history, so we should be able to bring back good stuff that gets stepped on.  --[[User:Rdm|Rdm]] 17:21, 28 April 2011 (UTC)

:::::: I think the rest of this discussion should be on a category page.  Probably not Encryption (too narrow). I'll get something going.  --[[User:Dgamey|Dgamey]] 23:00, 28 April 2011 (UTC)


### Split the task?


:::And given that the task seems to be about doing RSA, does the Python example really need a hundred lines of UI? [[User:Pjt33|Pjt33]] 12:56, 2 April 2011 (UTC)
::::Probably not. I really like that there are two different UI approaches shown, one using a library and one not, but that's probably better done in a task more targeted for UI operations. Which we don't have enough of, IMO. They should be created if people have the relevant interest. --[[User:Short Circuit|Michael Mol]] 14:22, 2 April 2011 (UTC)

: The description does need enhancement and citation per Dkf
::  A reference to [[wp:RSA|RSA]] for a start - yes it is there either added or missed by me at first read.
::  A caution that the modulus is a demonstration size only and reference to something on key sizes.  Real keys are much longer and according to NIST even 1024 bit keys are on their way out. I believe the correct NIST document is referenced here [[wp:Key_size|Key sizes]].  Possibly also the RSA challenges [http://www.rsa.com/rsalabs/node.asp?id=2092 RSA Key Factoring Challenge Archive]
::  A caution on cryptography like appears in the MD5 or MD5 implementation task (the later if I recall)
::  A note that the <strike>blocking</strike> encoding character set is arbitrary and that real implementations just encrypt the the underlying binary -- however as noted later RSA 129 used a plain text encoding scheme
::  --[[User:Dgamey|Dgamey]] 10:44, 21 April 2011 (UTC) update: --[[User:Dgamey|Dgamey]] 01:33, 26 April 2011 (UTC)

::: If blocking is arbitrary, perhaps blocking should be removed from the task description?  The Python implementations currently use a block size of 1 character, and the blocking code adds some significant complexity, so why not just get rid of it?  --[[User:Rdm|Rdm]] 11:45, 22 April 2011 (UTC)
:::: Blocking is needed to keep the encoded plain text shorter than the modulus.  It's really needed with a toy modulus like this.  --[[User:Dgamey|Dgamey]] 01:33, 26 April 2011 (UTC)

::::I've been thinking about this and I see two tasks here.  Let me explain. (I added the subtitle above)

:::::RSA should be about encryption/decryption.  The problem is the demonstration numbers are too small for multiple ascii characters.  Finding one or two sets of larger numbers would allow for this and remove some of the resulting constraints.  RSA has no problem with zeros. Nor does it char about ascii, ebcdic, double-byte, etc.  RSA also requires bignum support so having some numbers that exercise that would make sense.  Since some languages can't do this having several sets of key pairs of regular and bignum size would allow everyone to play.  Calling out the limitation should be enough.  I was poking around looking for some, thinking of the smaller RSA challenges (140 bit) but haven't yet found the whole set of numbers.  I think running a test message through a couple of different RSA key pairs would make a fine task.  Perhaps generating some small keys would be a good sister task.
::::::Neither of the current implementations uses ascii characters, instead both use an encoding that can represent 31 distinct characters.  This would support a block size of two characters.  (The J implementation used a block size dynamically determined based on the number of represented characters and the keys, and this was two characters for the example key.  I took that out though, when it seemed that no one else was going to budge on this issue, and the current J implementation uses a blocksize of 1 character -- which does have the advantage of simplicity.) --[[User:Rdm|Rdm]] 14:22, 25 April 2011 (UTC)

:::::: Some links.
:::::: [http://cisnet.baruch.cuny.edu/holowczak/classes/9444/rsademo/ java key gen demo]
:::::: [http://shop-js.sourceforge.net/crypto2.htm js key gen demo]
:::::: [http://www.prime-numbers.org/ prime number source] - I've requested a few over 5 billion and will post some

::::: I changed my mind both character encoding and plain text blocking are needed for a small n.  
:::::I also agree the huge UI thing didn't belong here.  Possible another task.

:::: --[[User:Dgamey|Dgamey]] 14:15, 22 April 2011 (UTC) updated: --[[User:Dgamey|Dgamey]] 01:33, 26 April 2011 (UTC)


###  RSA129 as an example 


It seems the RSA 129 challenge performed a character encoding as well ... hmmm

: [http://www.willamette.edu/~mjaneba/rsa129.html RSA 129 factors]
: [[wp:The_Magic_Words_are_Squeamish_Ossifrage|The RSA 129 text "The Magic Words are Squeamish Ossifrage"]]
: [http://www.math.okstate.edu/~wrightd/numthry/rsa129.html summaries of rsa 129 status reports and full details of keys (see below)]
: [http://www.math.okstate.edu/~wrightd/crypt/crypt-intro/node21.html More info on the challenge text and signature]
: [[wp:Modular_exponentiation|Modular exponentiation]] will challenge any naive implementation of RSA.  Your bignums may blow up decrypting the message above.  This would be a worthy prerequisite task.
: The RSA 129 character encoding was " "=0, A=1,.. Z=26 by 100's so that 200805 was THE.

-[[User:Dgamey|Dgamey]] 15:56, 22 April 2011 (UTC) update: --[[User:Dgamey|Dgamey]] 01:19, 26 April 2011 (UTC)


### = Question for the task author and other editors =


I have enough information to define a task to provide a demonstration task that validates RSA129 implementation.  I can create this as a separate task or this task could be modified.  I suspect the proper approach would be to create a new one (RSA/129Implementation).  However, before I do this, I should let you know that it would cover a lot of the elements of this version of the task.  I've noted new elements below:
*  RSA encryption / decryption
*  modular exponentiation with large integer support - new/modified
*  message encoding/decoding - different/clarified
*  use the keys, text, and numbers from the 1977 RSA129 challenge from SciAm - new 
*  validating that all the numbers, text, and routines work correctly - new/different
*  message blocking - not needed for messages shorter than about 64 characters - could be left out or optional - just calculating a safe blocking factor would do

Just wondering the best way to proceed.  
Should we put a note to task writers not to proceed at this point until this is clarified or changed?

Since the history shows you weren't signed in when you created the task I couldn't contact you directly. 
Thanks --[[User:Dgamey|Dgamey]] 11:40, 25 April 2011 (UTC)
: I suspect the author is Erasmus,  is it possible to chat?  Can you e-mail me?  --[[User:Dgamey|Dgamey]] 02:45, 27 April 2011 (UTC)

=== RSA 129 - Final Answer (April 27, 1994) ===
We are happy to announce that

RSA-129 = 11438162575788886766923577997614661201021 82967212423625625618429357069352457338978 30597123563958705058989075147599290026879543541

= 3490529510847650949147849619903898133417764638493387843990820577 * 32769132993266709549961988190834461413177642967992942539798288533

The encoded message published was

968696137546220614771409222543558829057599911245743198746951209 30816298225145708356931476622883989628013391990551829945157815154

This number came from an RSA encryption of the `secret' message using the public exponent 9007. When decrypted with he `secret' exponent

106698614368578024442868771328920154780709906633937862801226224 496631063125911774470873340168597462306553968544513277109053606095

this becomes

20080500130107090300231518041900011805 0019172105011309190800151919090618010705

Using the decoding scheme 01=A, 02=B, ..., 26=Z, and 00 a space between words, the decoded message reads

THE MAGIC WORDS ARE SQUEAMISH OSSIFRAGE

To find the factorization of RSA-129, we used the double large prime variation of the multiple polynomial quadratic sieve factoring method. The sieving step took approximately 5000 mips years, and was carried out in 8 months by about 600 volunteers from more than 20 countries, on all continents except Antarctica. Combining the partial relations produced a sparse matrix of 569466 rows and 524338 columns. This matrix was reduced to a dense matrix of 188614 rows and 188160 columns using structured Gaussian elimination. Ordinary Gaussian elimination on this matrix, consisting of 35489610240 bits (4.13 gigabyte), took 45 hours on a 16K MasPar MP-1 massively parallel computer. The first three dependencies all turned out to be `unlucky' and produced the trivial factor RSA-129. The fourth dependency produced the above factorization.

We would like to thank everyone who contributed their time and effort to this project. Without your help this would not have been possible.

Derek Atkins

Michael Graff

Arjen Lenstra

Paul Leyland 

--[[User:Dgamey|Dgamey]] 15:39, 24 April 2011 (UTC)

==Blocking?==
"This yields two blocks of numbers ..." I can see that a series of numbers are produced, but the method of splitting into blocks is not given. --[[User:Paddy3118|Paddy3118]] 02:45, 26 March 2011 (UTC)

The blocking code is more complicated than the encryption code.  But:

* When converting letters to numbers, the numbers should be non-zero.
* If if X is the largest number value, then pick the largest K such that (1+X)^K < N (where N is from the key).  K is the number of letters represented in a block.
* If v is an array of numbers repesenting the letters in a block, the numeric value of the block itself is can be computed (using psuedo-C or maybe psuedo-javascript):

    block= 0;
    for (int i= 0; i<v.length; i++) {
        block= v[i]+(X+1)*block;
    }

* To go the other direction:

    for (int i= v.length-1; i >= 0; i--) {
        v[i]= block % (X+1);  /* remainder function corresponding to division on next line */
        block= block / (X+1); /* integer division */
    }

--[[User:Rdm|Rdm]] 03:31, 26 March 2011 (UTC)

==RSA in J==
I may simply be naive, but it seems to me that the example doesn't code "hi there" correctly. The output is given as "695 153 2377 260". I tried to decrypt those blocks, but i can only recover gibberish from them. Even abandoning my python code and simply doing the modular exponentiation yields the same results. I get "575, 1230, 2387, 205" if i split "hi there" into 4 blocks. Can you explain how exactly you are encoding and decoding? I am not familiar with J, so i cant really tell whats going on in your code. --[[User:Erasmus|Erasmus]] 03:04, 30 March 2011 (UTC)
:I believe the pseudo code I posted, above, matches the process I used in J, though I cannot actually run the pseudo code, and it might be buggy.  But I have updated the J entry with a blocking example.  So, for example:  'h' is letter 8 and 'i' is letter 9 and 'hi' is thus (32*8)+9 or 265.  But perhaps we should be using a different blocking algorithm? --[[User:Rdm|Rdm]] 12:26, 30 March 2011 (UTC)

:I should perhaps also note that I cannot get the python example to work.  It fails for me with the message <code>ImportError: No module named tkinter</code>.  But these work for me:

:
```python
>>>
 import _tkinter
>>> import Tkinter
>>> Tkinter._test()
```


:And the python implementation does not supply any canned examples.

:So, anyways, I have not tested against the python code. --[[User:Rdm|Rdm]] 17:35, 30 March 2011 (UTC)

::Ah...that explains it. Our blocking methods were just entirely different. I recovered 265, but i didnt know how to turn that back to letters. My code blocks "hi" as "0809", since "h" is "08" and "i" is "09". If "f" is the number of digits in "n", my python code only places (f/2)-1 letters per block. I realize this doesn't make sense for a small n, but at larger n it isn't noticeable, and it prevents my blocks from being larger than "n".

::I have uploaded a version of my code that doesn't use the Tkinter window. I find running it from python IDLE seems to work best, since i can copy paste plaintext or ciphertext to and from the window. --[[User:Erasmus|Erasmus]] 01:18, 1 April 2011 (UTC)

:::Ok, I think I understand your approach now.  The reason I went the way I did was because I felt "letters" was somewhat arbitrary.  I could, in principle, use arbitrary ascii or even unicode.  (If I used utf8 instead of wide characters, some characters would get split across multiple blocks, but except for the garbage padding at the end, that should not be a problem.  I would need to do something special to deal with the errors that could be generated by using arbitrary codes in the padding at the end, but I have not thought that far ahead.)  That said, I see that the task description has changed, so I will need to update the J implementation. --[[User:Rdm|Rdm]] 11:55, 1 April 2011 (UTC)
:::Actually, I have a problem with this blocking mechanism.  N is 2537 while ' t' encodes as 3120, which means that the RSA algorithm decodes it as 583.  It's possible to work around this for cases where the decoding produces a non-legal character, but I believe that that kind of special case code makes this not be a general case implementation of RSA.  However, I am still not able to test my example phrase ('hi there') against your code -- this time I had to do a fresh python install (the machine I was using did not have it installed) and after installing it and running your code, I get an error when I try to use the new program:

:::
```python
Traceback (most recent call last):
  File "rsa.py", line 154, in <module>
    if mm.lower() == 'encrypt':
AttributeError: 'function' object has no attribute 'lower'
```
 --[[User:Rdm|Rdm]] 12:34, 1 April 2011 (UTC)

:::Ok, so I editted all instances of .lower out of the python code.  However, I should note that you are apparently only using one letter per block.  That means that the theoretical problem I mentioned is not an issue for you.  But that also means that your python implementation is just doing a simple letter substitution code.  --[[User:Rdm|Rdm]] 12:53, 1 April 2011 (UTC)

::::The .lower() may be something added in python 3. I forgot to mention that being new to coding, nobody told me that python 2 was still more widely used, so this is all done in the newest version of python. The .lower() function just changes a string to all lower case letters, so it took out the case sensitivity when my code tries to pair the letter with a number.

:::: For larger n, the blocks grow. Since this n is only 4 digits long, each block is one letter. If n were 20 or 21 digits, each block would contain 9 characters. That was my way of making sure the blocks were never larger than n, since each character for me converts to 2 digits.   --[[User:Erasmus|Erasmus]] 00:03, 2 April 2011 (UTC)
~

::::: Ok, but as specified, the task is to substitute the sequence of letters <code>'abcdefghijklmnopqrstuvwxyz,.!? '</code> and replace each of them with the corresponding number from this list: <code>1 581 1087 140 205 2371 1125 156 1864 2403 821 2497 2038 1616 2116 1841 959 2222 2299 793 41 45 2475 2130 1433 1836 1642 206 577 1488 384</code>.  Decryption is looking up those numbers and substituting for the corresponding letters.  The padding is irrelevant noise.  And, as specified, you do not have to implement RSA at all to accomplish this task.  --[[User:Rdm|Rdm]] 00:58, 2 April 2011 (UTC)
