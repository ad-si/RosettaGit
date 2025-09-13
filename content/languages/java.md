+++
title = "Java"
description = ""
date = 2018-09-04T20:57:52Z
aliases = []
[extra]
id = 1700
[taxonomies]
categories = []
tags = []
+++
[](https://rosettacode.org/wiki/runs_on_vm::java_virtual_machine)
The '''Java''' programming language, developed by [Sun Microsystems](https://rosettacode.org/wiki/Sun_Microsystems), is a language aimed at allowing "high-performance", virtual application development.

Java source files (.java files) are typically [compiled](https://rosettacode.org/wiki/Compiler) to an intermediate [bytecode](https://rosettacode.org/wiki/bytecode) (all platform) executable (.class files) and executed by a [Java Virtual Machine](https://rosettacode.org/wiki/Java_Virtual_Machine).
Most modern JVMs further compile the bytecode into your processor's native machine code during execution.
This native code translation is mostly done via a [JIT](https://rosettacode.org/wiki/Just-In-Time) compiler that is built in the JVM. Some Java compilers (such as [GCJ](https://rosettacode.org/wiki/GCJ)) can compile Java code to native machine code ahead-of-time rather than just-in-time.

The primary benefits of Java are:
* Cross-[platform](https://rosettacode.org/wiki/platform) (Write Once, Run Anywhere (if all of the libraries are ported))
* Comprehensive class library (which is thoroughly [http://www.oracle.com/technetwork/java/javase/documentation/index.html documented])
* [Automatic memory management](https://rosettacode.org/wiki/garbage_collection)
* Large user community
* Well supported by the [open source](https://rosettacode.org/wiki/open_source) community and commercial industry ([Sun](https://rosettacode.org/wiki/Sun_Microsystems), [IBM](https://rosettacode.org/wiki/IBM), [Oracle](https://rosettacode.org/wiki/Oracle), HP, BEA, [Apple Inc](https://rosettacode.org/wiki/Apple_Inc), etc.)

Java is used in a variety of environments including desktop applications, embedded device (PDA and wireless phone) applications, server-side web page serving applications (as [JSP](https://rosettacode.org/wiki/Java_Server_Pages)), and applets embedded in web pages.

There are a variety of arguments regarding Java's performance compared to other popular languages like [C++](https://rosettacode.org/wiki/C++). Some come to the conclusion that the programmer's choices make a bigger difference on performance in each language. Most show that each language is better than the other at specific types of operations (e.g. Java for memory allocation, C++ for numerical operations).
An extensive summary of comparisons between Java and C++ can be found [on Wikipedia](https://en.wikipedia.org/wiki/Comparison_of_Java_and_C++).

According to some [http://www.tiobe.com/tpci.htm sources], Java is currently one of the most popular programming languages in the world.

Useful Java links:
* [http://www.java.com java.com]
* [http://www.java.net java.net]
* [http://openjdk.java.net OpenJDK]

## Todo
[Reports:Tasks_not_implemented_in_Java](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Java)


## Merged content



In this implementation of [Brainfuck](https://rosettacode.org/wiki/Brainfuck) in [Java](https://rosettacode.org/wiki/Java), the code is read in all at once and checked for uneven brackets (unequal amounts of [ and ] commands). If that error occurs, the code will obviously not be run.

Under the hood, the program memory is an <tt>ArrayList</tt> of Integers which expands "infinitely" (limited by your system's memory) to the right (still [http://en.wikipedia.org/wiki/Turing_completeness Turing complete]). So, if the pointer moves past zero to the left, the program will exit and a "Pointer out of range" error message will be displayed.

Due to the <tt>BufferedReader</tt> input class, return characters ([http://www.asciitable.com ASCII] 10 and 13) are ignored on input (the , command), but are not ignored on output (the . command). In order to input anything, you'll need to append a new line.

Loops are handled in the <tt>jumpForward</tt> and <tt>jumpBack</tt> methods. In each method, the bracket that the code pointer is on is counted as 1 (the counter is initialized to 1). The method then moves the code pointer forward or back and counts other brackets along the way. If it finds a bracket of the same type, 1 is added to the counter; if it finds a bracket of the other type, 1 is subtracted from the counter. The search continues until the counter reaches 0. For jumping forward, the code pointer must be allowed to skip the ending bracket, or else it will jump back on the next execution iteration. For jumping back, the pointer can remain at the starting bracket. So, <tt>jumpBack</tt> offsets the next code pointer increment to make sure the check for 0 happens.

More detailed information about the rest of the code can be found in the comments throughout it.<br clear=all>
```java5
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

/**
 * Interprets Brainf**k source from std in or a file.
 *
 * @author Mike Neurohr
 */
public class BF{
	private static int pointer;//memory pointer
	private static int codeIndex;//instruction number
	private static final int INITIAL_SIZE= 1000;//initial size of memory
	private static final String instChars= "[]<>,.+-";//valid code characters

	/**
	 * The main program. Holds the input loop and error-handling.
	 *
	 * @param args 0: Source file    others: ignored
	 */
	public static void main(String[] args){
		BufferedReader source;
		try{
			if(args.length == 0){//if no file specified
				source= new BufferedReader(new InputStreamReader(System.in));
			}else{
				source= new BufferedReader(new FileReader(args[0]));
			}
			char instruction;

			//holds the cleaned up code
			StringBuilder code= new StringBuilder();

			//count for loop syntax errors
			int bCnt= 0;

			//read whole source in
			for(instruction= (char)source.read(); source.ready(); instruction=
					(char)source.read()){
				//remove extra characters
				if(instChars.contains(instruction + "")){
					//add the instruction to the clean code
					code.append(instruction);
					if(instruction == '[') bCnt++;
					if(instruction == ']') bCnt--;
				}
			}
			if(bCnt != 0){//if there is a bracket with no match
				System.err.println("Error: Uneven brackets.");
				return;
			}

			//set up input for ,
			BufferedReader input=
					new BufferedReader(new InputStreamReader(System.in));
			//set up memory
			ArrayList<Integer> memory= new ArrayList<Integer>(INITIAL_SIZE);
			memory.add(new Integer(0));

			pointer= 0; //initialize pointer
			//loop through the cleaned up code and execute
			for(codeIndex= 0;codeIndex < code.length();codeIndex++){
				instruction= code.charAt(codeIndex);
				switch(instruction){
				case '+':
					memory.set(pointer, memory.get(pointer) + 1);
					break;
				case '-':
					memory.set(pointer, memory.get(pointer) - 1);
					break;
				case '.':
					System.out.print((char)memory.get(pointer).intValue());
					break;
				case ',':
					input(memory, input);
					break;
				case '[':
					if(memory.get(pointer) == 0){
						//skip the code inside the loop
						jumpForward(code);
					}
					//if the loop can continue...
					//we don't need to check other things
					break;
				case ']':
					if(memory.get(pointer) != 0){
						//go back to the corresponding [ to check
						jumpBack(code);
					}
					//if the loop should stop...
					//we don't need to check other things
					break;
				case '>':
					pointer++;
					//gets rid of NullPointerExceptions
					while(pointer + 1 > memory.size()) memory.add(0);
					break;
				case '<':
					if(--pointer < 0){
						//can't do it
						System.err.println("Pointer out of range (negative).");
						return;
					}
				default:
					//No other characters left after the cleaning
				}
			}
		}catch(FileNotFoundException e){
			System.err.println("Error opening file: " + args[0] + ".");
		}catch(IOException e){
			System.err.println("Error on input.");
		}
	}

	public static void jumpBack(StringBuilder code){
		//initial count for the bracket we're on
		int bracketCnt= 1;
		//count brackets until the corresponding [
		while(codeIndex >= 0 && bracketCnt != 0){
			codeIndex--;
			char inst= code.charAt(codeIndex);
			if(inst == '[') bracketCnt--;
			if(inst == ']') bracketCnt++;
		}
		//"- 1" to offset the next "codeIndex++".
		codeIndex= codeIndex - 1;
	}

	public static void jumpForward(StringBuilder code){
		//initial count for the bracket we're on
		int bracketCnt= 1;
		//count brackets until the corresponding ]
		while(codeIndex < code.length() && bracketCnt != 0){
			codeIndex++;
			char inst= code.charAt(codeIndex);
			if(inst == ']') bracketCnt--;
			if(inst == '[') bracketCnt++;
		}
	}

	public static void input(ArrayList<Integer> mem, BufferedReader input)
			throws IOException{
		int val;
		//read until something comes in other than return chars
		for(val= input.read(); val == 10 || val == 13; val= input.read());
		mem.set(pointer, new Integer(val));
 	}
}
```

