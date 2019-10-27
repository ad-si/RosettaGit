+++
title = "Category:Cos"
description = ""
date = 2017-07-08T03:12:38Z
aliases = []
[extra]
id = 21513
[taxonomies]
categories = []
tags = []
+++

Character on stack programming language(cos)


Hello world is "hello world" Z


The Z is a command that terminates the program execution.


		COS Language<br />
<br />
Character on stack programming language has 39 commands:<br />
<br />
ABDFLMPRTWZ!"#$%'()*+,-./:;<=>?@[\]_{|}<br />
<br />
1.	stacks:		\$%@RDP<br />
2.	arithmetic:	+-*/<br />
3.	input & output:	.:,;WF"<br />
4.	flow control:	<>L_![]<br />
5.	decisions:	=?|<br />
6.	code:		()#Z' 0 to 9 a to z<br />
7.	memory:		{}AM<br />
8.	miscellenious:	BT<br />
<br />
Parameters of cos:	p means integer<br />
			c means source text character<br />
			a means integer array<br />
<br />
Any ascii character including whitespace that is not part of cos<br />
language is ignored by interpreter. Its presence generates no error. Small<br />
letter ascii characters are used as parameters for some commands e.g variables.<br />
<br />
Stacks<br />
<br />
Char	Name		Syntax		Result on data stack<br />
<br />
\	swap		2 3\		3 2<br />
$	duplicate	2$		2 2<br />
%	drop		2 3%		2<br />
@	rotate		2 3 4@		3 4 2<br />
R	rstack		4R			(4 is moved to return stack)<br />
D	dstack		D		4	(4 is back on the data stack)<br />
P	pick		6 7 8 9 2P	6 7 8 9 7<br />
<br />
Note: 0P is the same as $. And 1P is same as over in forth language<br />
<br />
Cos has 5 stacks: source, data, return, variable, & array stack.<br />
Return stack used to temporarily store values from data stack and<br />
to store return addresses of function calls.<br />
<br />
All stacks are integer stacks and are made from	main stack. Therefore they<br />
can be manipulated with same commands. E.g it's possible to fetch from the<br />
array cells using P	<br />
<br />
Arithmetic<br />
<br />
Char	Name		Syntax		Result on data stack<br />
<br />
+	plus		2 3+		5<br />
-	minus		'da'-		3	(ascii 100 - ascii 97=3)<br />
*	times		7$*		49	(7 was duplicated 7*7=49)<br />
/	divide		9 9 2#3/	33	(99/3=33)<br />
<br />
All arithmetic is integer. Therefore 5 2 / will give 2 not 2.5<br />
<br />
To input negative number write zero,the number,&subtract e.g 09- (-9)<br />
<br />
Cos uses a signed integer as basic data type. It is -2147483648 to 2147483647<br />
<br />
Input/Output<br />
Char	Name		Syntax		Result<br />
<br />
.	printnumber	4.		(the 4 will be printed on sreen)<br />
:	printchar	7:		(a beep will be heard)"1W<br />
"	printstring	"Hello world!"	(even whitespace characters are printed)<br />
,	inputnumber	,(only one number figure put on data stack. To get more<br />
			use more commas e.g ,,2# creates 2 digit number e.g 45)	<br />
;	inputchar	(Inputs ascii char on datastack. Does not input string)<br />
<br />
W	window		0W		(clears screen)<br />
			1W		(prints newline)<br />
			pp2W		(puts cursor at xy location on screen<br />
					e.g 9 8 2W will put cursor on<br />
					horizontal 9 and vertical 8)<br />
F	file		0F		(Input all contents of file data stack)<br />
			ap1F		(write chars into file e.g 'ek'21F<br />
					will write 2 chars into workfile)<br />
			ap2F		(appends file. Similar syntax to 1F)<br />
			3F		(puts size of file on datastack)<br />
			ap4F		Assign name to workfile. Default is the<br />
					loaded sourcefile<br />
Flow control<br />
<br />
Char	Name		Syntax		Result<br />
<	goleft		c<	(instruction pointer skips all letters<br />
				leftward till it finds the preceding letter<br />
				e.g 0b1+$.b< will print numbers infinitely)<br />
<br />
>	goright(skipright)		same as skipleft<br />
_	mark		_c(optional)	it marks a block in code L will jump to<br />
<br />
L	goto		pL or cL	0L means goto beginning of source<br />
					finds nth or corresponding character<br />
					parameter underscore<br />
<br />
!	call function 	p! or c!	Similar to goto but 0! impossible.<br />
<br />
[	function	[c(optional)	Code within square brackets is function<br />
<br />
]	return		]	Marks end of function. Control returned to<br />
				code that called it. In cos impossible to<br />
				nest functions but a function can call another<br />
				function or itself<br />
<br />
Decisions<br />
<br />
Char	Name		Syntax		Result<br />
<br />
=	equal		pp=		(puts 0 on stack if TOS is equal to NOS<br />
					1 if NOS is less than TOS<br />
					2 if NOS is greater than TOS)<br />
<br />
?	question(if)	pp?		(execute next command if NOS is equal<br />
					to TOS else execution skipped untill<br />
					| is found)<br />
<br />
|	endif		|		Execution always continues from here<br />
<br />
Note:	cos has no break or exit commands so to achieve break manually jump<br />
	the command after | by using brackets or skip or goto<br />
<br />
Cos does not support decision nesting. Use gotos to achieve and, or etc logical<br />
conditions<br />
<br />
Code<br />
Char	Name		Syntax		Result<br />
<br />
(	comment		(comment)	Code between the brackets not executed<br />
					It is possible to execute that code if<br />
					a flow command put instruction pointer<br />
					inside the brackets<br />
#	Number		ap#		Creates a number from a given array of<br />
					numbers e.g 9 8 7 3# will form 987<br />
'	Literal		'literal(s)'	All asciis inside the single quotation<br />
					marks is put on data stack as literal<br />
					ascii characters. Can be one character<br />
					or many e.g 'a'(97) or 'b1c' (98 49 99)<br />
Z	End		Z		Stops code execution<br />
<br />
Note: In cos 4g6k- the g and k will be ignored since they are not part of the<br />
language. The execution will result in -2 as answer. Numbers should be separated<br />
e.g 100 90 - will give 10 as an answer<br />
<br />
Cos recognises 0 to 9 and it puts these chars on data stack. A number is always<br />
entered as an array of numeral characters. To enter thirty six either write 36 <br />
or 6 6* or '$'($ is 36 ascii) or 3 6 2# or 6$*.<br />
<br />
Memory<br />
Char	Name	Syntax		Result<br />
<br />
{	assign	pp{ or pc{	TOS integer is moved from data stack to an<br />
				address in the memory array<br />
}	fetch	p} or c}	Integer is copied from address in array memory<br />
				to be TOS data stack<br />
<br />
A	array	app0A		Assigns numbered integer into arraymemory cells<br />
		ppp1A		Assign 1 integer into a memory array<br />
		pp2A		Copies integers from array to data stack.<br />
		pp3A		Copies 1 integer from array cells to data stack<br />
<br />
M	memory	0M		machine call the workfile memory address<br />
		1M		print the memory image of cos interpreter<br />
<br />
Note: 	The character parameter is any char from a to z. There are 27 address<br />
	memory cells from 0 to 27 (0,a to z = 27 cells)  From 28 that's arrays<br />
<br />
Memory management is done by manipulating data stack. It contains program code, variables, arrays, primary stack, and return stack.<br />
<br />
{, } can be used to self-modify code. Thus cos is turing complete.<br />
<br />
The default address<br />
	variable stack:	0 to 27<br />
	array stack:	28 - programesize+45000<br />
	primary stack:	45027-programsize+30000<br />
	return stack:	75027-programsize+1000<br />
	source stack:	76027-programsize<br />
<br />
	stack(0)	0	1399428<br />
	stack(76027)	76027	1703536<br />
	origin:		76028	1703540<br />
	rp:		76029	1703544<br />
	sp:		76030	1703548<br />
	ip:		76031	1703552<br />
	tc:		76032	1703556<br />
	sc:		76033	1703560<br />
	fc:		76034	1703564<br />
	ansn:		76035	1703568<br />
	an:		76036	1703572<br />
	ab:		76037	1703576<br />
	ac:		76038	1703580<br />
	workfile:	76041	1703592<br />
		<br />
Miscellenious<br />
<br />
Char	Name		Syntax		Result<br />
<br />
B	boolean		pp0B		NOS and TOS<br />
			pp1B		NOS or TOS<br />
			p2B		not TOS<br />
<br />
	shift		pp3B		NOS shiftleft TOS<br />
			pp4B		NOS shiftright TOS<br />
<br />
T	time		0T		Puts time on data stack<br />
			p1T		Random number from 0 to p<br />
			p2T		Pauses execution for a p<br />
					micro-seconds<br />
			<br />
			p3T		p is number of microsends to<br />
					pause. Afterward a key in<br />
					keyboard buffer is fetched<br />
					if no key then zero is stacked
