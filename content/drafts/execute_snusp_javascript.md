+++
title = "Execute SNUSP/JavaScript"
description = ""
date = 2010-02-06T14:35:40Z
aliases = []
[extra]
id = 3205
[taxonomies]
categories = []
tags = []
+++

{{implementation|SNUSP}}{{collection|RCSNUSP}}
This [[JavaScript]] code is the core of a Modular SNUSP interpreter and debugger designed to be embedded on a [http://www.quirkster.com/snusp/snusp-js.html web page].


```javascript
// to be run from a page having fields with these ids:
//   raw:	large textarea for code input
//   code:  empty PRE block for showing the program execution
//   input:	single line, from which ',' gets input
//   eof:       checkbox, if checked return 0 on EOF else halt
//   output:	PRE block, destination for output of '.'
//   mem:	single line, final contents of memory

// buttons can execute these commands:
//   format:  copy user input in raw to code
//   snusp:   run loop that runs to completion
//   step:    single step and highlight current position
//     uses number from field id "repeat" to step n times
//   out:     step until we unnest (hit '#' character)
//   slow:    toggle running steps on a timer
//     sets milliseconds per step from field id "speed" 

var code = "#";     // formatted code
var width;          // length of each line in code
var ip = 0;         // current instruction within code (mod by $, ?, !, #)
var dir = 1;        // current direction (mod by /, \, #)

var data = [0];     // data array (mod by +, -)
var dp = 0;         // index into data (mod by <, >)

var inp = 0;        // current input character (fetch with ,)
var quit = 0;       // termination flag

var stack = [];     // call stack (mod by @, #)

var commands = {
	'>':function() { if (++dp >= data.length) data[dp]=0 },
 	'<':function() { if (--dp < 0) quit++ },
 	'+':function() { ++data[dp] },
	'-':function() { --data[dp] },
 	'/':function() { dir = -width / dir },
 	'\\':function() { dir = width / dir },
 	'!':function() { ip += dir },
 	'?':function() { if (!data[dp]) ip += dir },
 	',':function() {
		data[dp] = document.getElementById("input").value.charCodeAt(inp++);
		if (isNaN(data[dp])) {   // EOF
			--inp;
			data[dp] = 0;
			if (!document.getElementById("eof").checked) quit++;
		}
 	},
	'.':function() {
		var s = document.getElementById("output").innerHTML
		      + String.fromCharCode(data[dp]);
		s = s.replace(/\n/g,"
").replace(/ /g," ");
		document.getElementById("output").innerHTML = s;
	},
	'@':function() { stack.push(ip+dir, dir) },
	'#':function() { if (stack.length) { dir=stack.pop(); ip=stack.pop(); } else quit++; },
	'\n':function() { quit++ }		// left or right edge
};

var spaces = "  ";
function format(id) {
	// format the buffer to evenly pad the line lengths
	var el = document.getElementById(id);
	var value = el.value || el.textContent || el.innerText;
	var lines = value.split('\n');
	width = 0;
	for (var i in lines)
		width = Math.max(width, lines[i].length);
	while (spaces.length < width)
		spaces += spaces;
	for (var i in lines)
		lines[i] += spaces.substring(0, width - lines[i].length);
	code = lines.join('\n');
	width++;

	// show the formatted code in a PRE block (id=code)
	init();
	dump();
	window.scroll(0,document.getElementById("code").offsetTop);
}

function init() {
	inp = 0; quit = 0; dp = 0; dir = 1;
	data = [0];
	stack = [];
	ip = code.indexOf('$');  if (ip<0) ip=0;
	document.getElementById("output").innerHTML = "";
}

function done() {
	return quit || ip<0 || ip>=code.length;
}

function body() {
	var fn = commands[code.charAt(ip)];
	if (fn) fn();
	ip += dir;
}

function encode(s) {
	var e = s.replace(/&/g, "&amp;");
	    e = e.replace(/</g, "&lt;");
		e = e.replace(/>/g, "&gt;");
		e = e.replace(/ /g, " ");
	return  e.replace(/\n/g, "
");
}
function dump() {
	//TODO: go through stack and color each return point green
	document.getElementById("code").innerHTML
		= encode(code.substring(0,ip))
		+ '<span style="background: pink">'
		+ encode(code.charAt(ip))
		+ "</span>"
		+ encode(code.substring(ip+1));
	document.getElementById("mem").value = data.join();
}

var tid = 0;
function step() {
	var n = document.getElementById("repeat").value - 0;

	if (done()) init();
	while (--n>=0 && !done())
		body();

	if (done() && tid) slow();	// stop timer
	dump();
}

function out() {
	var d = stack.length;
	while (stack.length>=d && !done())
		body();
	dump();
}

function slow() {
	if (tid) {
		clearInterval(tid);
		tid = 0;
		document.getElementById("slow").value = "Slow";
	} else {
		var n = document.getElementById("speed").value - 0;
		tid = setInterval(step, Math.max(n,10));
		document.getElementById("slow").value = "Stop";
	}
}

function run() {
	while (!done())
		body();
	dump();
}
```

