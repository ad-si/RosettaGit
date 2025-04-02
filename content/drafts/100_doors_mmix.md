+++
title = "100 doors/MMIX"
description = ""
date = 2010-01-08T17:20:04Z
aliases = []
[extra]
id = 5316
[taxonomies]
categories = []
tags = []
+++

*Part of: [100 doors](/tasks/100 doors)*

***not optimized**

```mmix
N	IS	100		% predefined number of doors
// registers
t	IS 	$255		% pointer
i	GREG			% counter
p	GREG			% door iterator
d	GREG			% door status
r	GREG			% temp

	LOC	Data_Segment
DOORS	BYTE	0		% status doors
	LOC	DOORS+N
dt	GREG	@
ptr	GREG	DOORS-@		% door locater
b4d1	GREG	DOORS-1-@	% locater with offset -1

BUF	TETRA			% print buffer, just enough room for	
%				  space, '1' or '0', and string terminater 0
	LOC	#1000
Main 	SET	i,0		% prepare counter
1H	INCL	i,1		% WHILE i <= N DO 
	CMP	t,i,N		%
	BP	t,5F		% 
	SET	p,b4d1		%  reset door iterator
3H	ADD	p,p,i		%  update door iterator
	BNN	p,1B		% IF p >= 0 TEN next run
	LDBU	d,dt,p		% ELSE get door status
	XOR	d,d,1		%  toggle status
4H	STBU	d,dt,p		%  replace status
	JMP	3B		% ENDWHILE

% Output 10 / line --> stdout
	GREG	@
Title	BYTE	"100 doors: open = 1..."
NewLn	BYTE	#a,0
Blank	BYTE	" ",0
5H	LDA	t,Title		% display title
	TRAP	0,Fputs,StdOut
	SET	p,ptr		% DO
3H	LDBU	d,dt,p		%  get status
0H	GREG	#20300000
	STT	0B,BUF		%  prepare buffer
	LDA	t,BUF+1		%  points to temp door(i) status
	INCL	d,'0'		%  convert to ascii
	STBU	d,t,0		%  place in buffer
	LDA	t,BUF
	TRAP	0,Fputs,StdOut	%  print door status
	INCL	p,1		%  next door
	DIV	d,p,N/10
	GET	r,rR
	BNZ	r,4F		%  if 10 doors displayed then output NL
	LDA	t,NewLn		%   else next door
	TRAP	0,Fputs,StdOut
4H	PBNZ	p,3B		% WHILE doors to go
	TRAP	0,Halt,0	% exit
```


***optimized**

```mmix
% Rosetta Code 100 Doors optimized
N	IS	100		% predefined number of doors
// registers
t	IS 	$255		% pointer
r	GREG			% temp
i	GREG			% step counter
p	GREG			% door iterator
d	GREG			% door status

	LOC	Data_Segment
DOORS	BYTE	0		% status of N doors
	LOC	DOORS+N
dt	GREG	@
ptr	GREG	DOORS-@		% points to 1st door
BUF	TETRA	0		% print buffer	

	LOC	#1000		% locate program
			% main (argc, argv) {
Main 	SET	i,1		% initialize step counter
	SET	d,1		% door status 'open'
	SET	p,ptr		% initialize iterator
1H	BP	p,5F		% REPEAT UNTIL no more doors
	STBU	d,dt,p		%  set door 'open'
	INCL	i,2		%  counts as follows: 1, 3, 5, 7, ...
	ADD	p,p,i		%  update iterator
	JMP	1B 		% END

% Output 10 / line --> stdout
% same as not optimized
```

Assembling not opt. version:

```txt
~/MIX/MMIX/Progs> mmixal 100Doors.mms
```

Running program 100Doors:

```txt
~/MIX/MMIX/Progs> mmix 100Doors
100 doors: open = 1...
 1 0 0 1 0 0 0 0 1 0
 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 1 0 0 0 0 0
 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 1 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 1 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 1 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 1
```

