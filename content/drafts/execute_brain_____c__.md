+++
title = "Execute Brain****/C++"
description = ""
date = 2014-02-23T15:33:59Z
aliases = []
[extra]
id = 2264
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
This is an implementation of [[Brainf***]] originally written in [[C++]] by [[User:Short Circuit|Mike Mol]] and [[User:Mwn3d|Mike Neurohr]], for Rosetta Code.  It is licensed under the same license as Rosetta Code itself.

It may be fed BF instructions either through standard input (i.e. terminal keyboard input), or through a file named as the first command-line argument.

RCBF is intended for educational purposes only; There is no guarantee it won't lock up your computer. (Though Mike M thinks he's got that bug ironed out...) (We'll no it wasn't, but it should be now -- RdB)

{{works with|g++|4.1.3}}
plus the GNU C++ Standard Library

```cpp
// RCBF -- A free Brainfuck interpreter written for Rosetta Code (http://rosettacode.org)
// Created by Mike Mol and Mike Neurohr, and under the same license as Rosetta Code.
// Repaired by Robert de Bath -- but Ghods is it slow!

// Test with this Hello World, it breaks BF interpreters
//  >++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>]<<]>.+++++++..+++.>
//  >+++++++.<<<[[-]<[-]>]<+++++++++++++++.>>.+++.------.--------.>>+.>++++.

#include <list>
#include <iostream>
#include <fstream>
#define CLOSE 0
#define SUCCESS 1
#define BRANCH_NOT_FOUND 2
#define ERROR_BRANCH_OPEN_NOT_MATCHED 3
#define ERROR_BRANCH_CLOSE_NOT_MATCHED 4
#define ERROR_FILE_LOAD_FAILED 5

using namespace std;

// Define our machine
typedef char instruction_t;
typedef char memorycell_t;

typedef list<instruction_t> memory_t;
typedef memory_t::iterator memptr_t;

typedef list<memorycell_t> code_t;
typedef code_t::iterator codeptr_t;

struct branch_s
{
	code_t::iterator open;
	code_t::iterator close;
};

typedef list<branch_s> branch_t;


// Our recursive execute
int execute(istream *source, code_t &code, codeptr_t &pos, memory_t &memory, memptr_t &ptr, branch_t &branches);

// Our Brainf**k instructions
int increment_ptr(memory_t &memory, memptr_t &ptr);
int decrement_ptr(memory_t &memory, memptr_t &ptr);
int increment(memptr_t &ptr);
int decrement(memptr_t &ptr);
int output(memptr_t &ptr);
int input(memptr_t &ptr);
int jump_forward(istream* source, code_t &code, codeptr_t &pos, memory_t &memory, memptr_t &ptr, branch_t &branches);
int jump_back(codeptr_t &pos, memptr_t &ptr, branch_t &branches);

// utility function
int read_forward_to_branch_close( istream* source, code_t &code, codeptr_t &pos, branch_t &branches );
int get_forward_jump( code_t &code, codeptr_t &pos, branch_t &branches );

// Our starting function
int main(int argc, char* argv[])
{
	// Set up our virtual machine
	
	//memory
	memory_t memory;
	memory.push_back(0);
	memptr_t ptr = memory.begin();

	code_t code;
	codeptr_t pos = code.begin();

	branch_t branches;

	// Set up our code source
	ifstream file;

	istream *source;

	// If we were given a file to read, open it.
	if ( 1 < argc )
	{
		file.open(argv[1]);

		// If the file failed to load, we need to abort.
		if ( !file.is_open() )
		{
			cerr << "File load failed" << endl;
			return ERROR_FILE_LOAD_FAILED;
		}

		source = &file;
	}
	else
		source = &cin;

	while ( true )
	{
		// A temp variable to store our instruction
		instruction_t instruction;

		codeptr_t end = code.end();

		if( end == pos )
		{
			// Read in from a file
			*source >> instruction;

			// Read in our instruction
			if ( source->eof() )
			{
				// If it's a file, close it.
				ifstream* sourcefile = dynamic_cast<ifstream*>(source);

				// dynamic_cast returns 0 if the cast isn't legal.
				if ( 0 != sourcefile )
					sourcefile->close();
				return CLOSE;
			}

			// append it to our code stack
			code.push_back(instruction);
	
			pos = code.end();
			--pos;
		}

		// interpret this instruction
		int condition = execute( source, code, pos, memory, ptr, branches );

		// Did we encounter an error?
		if ( SUCCESS != condition )
			// An error was encountered.  We're aborting.
			return condition;
	}

	return CLOSE;
}

// Where we actually interpret code
int execute(istream* source, code_t &code, codeptr_t &pos, memory_t &memory, memptr_t &ptr, branch_t &branches)
{
	int ReturnVal = SUCCESS;
	// Execute the instruction
	
	switch ( *pos )
	{
		case '>':
			ReturnVal = increment_ptr(memory, ptr );
			break;
		case '<':
			ReturnVal = decrement_ptr(memory, ptr );
			break;
		case '+':
			ReturnVal = increment( ptr );
			break;
		case '-':
			ReturnVal = decrement( ptr );
			break;
		case '.':
			ReturnVal = output( ptr );
			break;
		case ',':
			ReturnVal = input( ptr );
			break;
		case '[':
			ReturnVal = jump_forward(source, code, pos, memory, ptr, branches);
			break;
		case ']':
			ReturnVal = jump_back(pos, ptr, branches);
			break;
	}

	// Increment the code pointer
	++pos;

	return ReturnVal;
}
int increment_ptr( memory_t &memory, memptr_t &ptr)
{
	// If we're at the end of our memory, or if we don't have any
	memptr_t end = memory.end();

	++ptr;

	if ( ptr == end )
	{
		// add a little more, and initialize it.
		memory.push_back(0);

		// And reset our memory pointer to point to the last real cell
		ptr = memory.end();
		--ptr;
	}

	return SUCCESS;
}

int decrement_ptr( memory_t &memory, memptr_t &ptr)
{
	// if we're at the beginning of our memory
	memptr_t begin = memory.begin();
	if ( ptr == begin )
	{
		// Add a little more, and initialize it.
		memory.push_front(0);
		ptr = memory.begin();
	}
	else
		// Just a normal decrement will be fine.
		--ptr;
	
	return SUCCESS;
}

int increment(memptr_t &ptr)
{
	// Increment the value at the memory address
	*ptr = *ptr + 1;

	return SUCCESS;
}

int decrement(memptr_t &ptr)
{
	// Decrement the value at the memory address
	*ptr = *ptr - 1;

	return SUCCESS;
}

int output(memptr_t &ptr)
{
	cout << *ptr;
	return SUCCESS;
}

int input(memptr_t &ptr)
{
	*ptr = cin.get();
	return SUCCESS; 
}

int jump_forward(istream* source, code_t &code, codeptr_t &pos, memory_t &memory, memptr_t &ptr, branch_t &branches)
{
	// First, is this branch recorded?  It needs to be.
	branch_t::iterator branch = branches.begin();
	branch_t::iterator end = branches.end();

	while ( ( branch != end ) && ( branch->open != pos ) )
		++branch;

	// If we hit the end of the branch list, it wasn't recorded.
	if ( branch == end )
	{
		// record it.  We may need to jump back here.
		branch_s rec;

		// We set open and close to the same value to signify that we don't yet know where the actual close is.
		rec.open = rec.close = pos;
		branches.push_back(rec);
	}
		
	// Find the closing jump
	int forward_result = SUCCESS;
	codeptr_t jump = pos;
	while( BRANCH_NOT_FOUND == ( forward_result = get_forward_jump( code,  jump, branches ) ) )
	{
		// We haven't read far enough forward to find the corresponding closing branch
		int read_result = read_forward_to_branch_close( source, code, jump, branches );
		if ( SUCCESS != read_result )
			return read_result;
	}

	if ( SUCCESS != forward_result )
		return forward_result;

	// We only jump forward if there is a 0 at the current memory pointer
	if ( 0 == *ptr )
		pos = jump;

	return SUCCESS;

}

int jump_back(codeptr_t &pos, memptr_t &ptr, branch_t &branches)
{

	// Find the corresponding close jump.
	branch_t::iterator branch = branches.begin();
	branch_t::iterator end = branches.end();

	while ( (branch != end ) && ( branch->close != pos) )
		++branch;

	if( end == branch )
		// We ran out of branches to check.  Did it not get registered?
		return ERROR_BRANCH_CLOSE_NOT_MATCHED;

	// We only jump if our current memory cell is not 0.
	if ( 0 != *ptr )
		pos = branch->open;

	return SUCCESS;
}

int read_forward_to_branch_close( istream* source, code_t &code, codeptr_t &pos, branch_t &branches )
{
	codeptr_t tmppos = pos;
	branch_t::iterator begin = branches.begin();
	branch_t::iterator end = branches.end();

	while ( true )
	{
		// read in instruction
		instruction_t instruction;
		*source >> instruction;

		if ( source->eof() )
		{
			ifstream* sourcefile = dynamic_cast<ifstream*>(source);

			// If the source is really a file...
			if ( 0 != sourcefile )
				sourcefile->close();

			return ERROR_BRANCH_OPEN_NOT_MATCHED;
		}

		// Add it to our code memory
		code.push_back( instruction );

		// Is it a branch instruction?

		if ( '[' == instruction )
		{
			// It's another forward jump.  We'll record it, but we'll keep searching.
			branch_s rec;
			codeptr_t tmppos = code.end();
			rec.open = --tmppos;
                        rec.close = rec.open;

			// Because we always encounter forward jumps before back jumps,
			// recording a forward jump will always mean adding another node
			// to the branches list
			branches.push_back(rec);
		}
		else if ( ']' == instruction )
		{
			// It's a backward jump.  Find the corresponding nest open
			branch_t::iterator branch = branches.end();
			--branch;
			
			// If the branch we're examining lacks a unique branch close, it's the one we're looking for.
			while ( ( begin != branch ) && ( branch->open != branch->close ) )
				--branch;

			if ( ( begin == branch ) && ( branch->open != branch->close ) )
				return ERROR_BRANCH_CLOSE_NOT_MATCHED;

			// Record the close jump
			branch->close = --(code.end());
			
			// We found a close jump, so we'll return; It might be the close jump the caller was looking for.
			return SUCCESS;
		}
		
	}
}

int get_forward_jump( code_t &code, codeptr_t &pos, branch_t &branches )
{
	branch_t::iterator branch = branches.begin();
	branch_t::iterator end = branches.end();
	while( ( end != branch ) && ( branch->open != pos ) )
		++branch;

	if( end == branch )
		// We ran out of branches.  This is bad.
		return ERROR_BRANCH_OPEN_NOT_MATCHED;

	if( branch->close != branch->open )
	{
		// We found our close branch.  Assign it to pos
		pos = branch->close;
		return SUCCESS;
	}

	// We never found a close branch
	return BRANCH_NOT_FOUND;
}
```

