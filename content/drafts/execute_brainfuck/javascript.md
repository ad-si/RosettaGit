+++
title = "Execute Brainfuck/JavaScript"
description = ""
date = 2010-02-06T14:22:10Z
aliases = []
[extra]
id = 2389
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
Presented here is the core of a Brainfuck interpreter/debugger written in [[JavaScript]] that is designed to run within a web page. The full source can be found at http://www.quirkster.com/iano/js/bf.html by viewing the page source.


```javascript
var code;           // formatted code
var ip = 0;         // current instruction within code
var nest = 0;       // current bracket nesting (for Out button)
var ahead = [];     // locations of matching brackets

var data = [0];     // data array (mod by +, -)
var dp = 0;         // index into data (mod by <, >)

var inp = 0;        // current input character (fetch with ,)
var quit = 0;       // termination flag

var commands = {
	'>':function() { if (++dp >= data.length) data[dp]=0 },
 	'<':function() { if (--dp < 0) quit++ },
 	'+':function() { ++data[dp] },
	'-':function() { --data[dp] },
 	'[':function() { if (!data[dp]) ip = ahead[ip]; else ++nest },
 	']':function() { if ( data[dp]) ip = ahead[ip]; else --nest },
 	',':function() {
		var c = document.getElementById("input").value.charCodeAt(inp++);
		data[dp] = isNaN(c) ? 0 : c;  // EOF: other options are -1 or no change
 	},
	'.':function() {
		var s = document.getElementById("output").innerHTML
		      + String.fromCharCode(data[dp]);
		s = s.replace(/\n/g,"
").replace(/ /g,"&amp;nbsp;");
		document.getElementById("output").innerHTML = s;
	},
};

function format(id) {
	var el = document.getElementById(id);
	code = el.value || el.textContent || el.innerText;
	// scan for matching braces
	var st = [], back, error = -1;
	ahead = [];
	for (ip=0; ip<code.length; ip++) {
		switch(code[ip]) {
		case '[':
			st.push(ip);
			break;
		case ']':
			if (st.length == 0) error = ip;
			back = st.pop();
			ahead[ip] = back;
			ahead[back] = ip;
			break;
		}
	}
	if (st.length > 0) error = st[0];
	init();
	if (error >= 0) {
		ip = error;	// highlight error
		alert("Unmatched '"+code[error]+"' at "+error+"!");
	}
	// show the  code in a PRE block (id=code)
	dump();
	window.scroll(0,document.getElementById("code").offsetTop);
}

function encode(s) {
	var e = s.replace(/&/g, "&amp;amp;");
	    e = e.replace(/</g, "&amp;lt;");
		e = e.replace(/>/g, "&amp;gt;");
		e = e.replace(/ /g, "&amp;nbsp;");
	return  e.replace(/\n/g, "
");
}
function dump() {
	document.getElementById("code").innerHTML
		= encode(code.substring(0,ip))
		+ '<span style="background: pink">'
		+ encode(code.charAt(ip))
		+ "</span>"
		+ encode(code.substring(ip+1));
	document.getElementById("mem").value = data.join();
}

function init() {
	inp = 0; quit = 0; dp = 0;
	data = [0]; ip = 0;
	document.getElementById("output").innerHTML = "";
}

function done() {
	return quit || ip >= code.length;
}

function body() {
        var fn;
        do {
          fn = commands[code.charAt(ip)];
	  if (fn) fn();
	  ++ip;
	} while (fn == null && !done());
}

// button handlers

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
	var d = nest;
	while (nest>=d && !done())
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

