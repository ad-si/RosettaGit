+++
title = "Execute SNUSP/Java"
description = ""
date = 2010-02-06T14:35:09Z
aliases = []
[extra]
id = 2846
[taxonomies]
categories = []
tags = []
+++

{{implementation|SNUSP}}{{collection|RCSNUSP}}
This [[Java]] implementation has all of the basic characters plus '<code>%</code>', '<code>@</code>', and '<code>#</code>' (characters from modular and bloated SNUSP found on [[eso:SNUSP|esolangs]]). The memory space grows to the right as needed, and the memory pointer cannot go negative. The program will exit if the memory pointer moves to a negative value or if the code pointer leaves the code space. The input ignores return characters because of the way <tt>BufferedReader</tt> is set up. The random command places a random ASCII value (code 0 through the value in the cell inclusive) in the current memory space.

The implementation comes in two classes: the main program and a code pointer class. The separation is for the easier addition of '<code>&</code>' (split) which could come in the future (the code pointer class would only need to extend <tt>Thread</tt> and code would need to be added to deal with the command). The main class decides on the input method (file or std in), reads the code, and starts the code pointer. It also figures out where the starting point in the code is ('<code>$</code>' or 0,0).<br clear=all>

{{works with|Java|1.5+}}

The main class:

```java5
import java.awt.Point;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

public class SNUSP{
	static Point start;
	public static void main(String[] args){
		try{
			start= new Point(0,0);
			BufferedReader in;
			ArrayList<String> code = new ArrayList<String>();
			if(args.length > 0){
				in= new BufferedReader(new FileReader(args[0]));
			}else{
				in= new BufferedReader(new InputStreamReader(System.in));
			}
			code= parse(in);
			CodePtr cp = new CodePtr(code, start);
			cp.run();
		}catch(FileNotFoundException e){
			e.printStackTrace();
		}catch(IOException e){
			e.printStackTrace();
		}
	}

	private static ArrayList<String> parse(BufferedReader in) 
	throws IOException{
		String line;
		ArrayList<String> code= new ArrayList<String>();
		while((line= in.readLine())!=null){
			code.add(line);
			if(line.contains("$")){
				start = new Point(line.indexOf('$'), code.size() - 1);
			}
		}
		return code;
	}
}
```

The code pointer class:

```java5
import java.awt.Point;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Random;

public class CodePtr{
	static final String instChars= "><.,/\\+-#@$%!?";//valid chars
	static final Random rand = new Random();
	ArrayList<String> code;//code array
	Direction dir;//current direction
	ArrayList<Character> mem;//memory space
	int memPtr;//memory pointer
	Point place;//code pointer
	LinkedList<Point> pStack;//code pointer stack
	LinkedList<Direction> dStack;//direction stack
	//input stream
	BufferedReader input= new BufferedReader(new InputStreamReader(System.in));

	public CodePtr(ArrayList<String> code, Point place){
		this.code= code;
		dir= Direction.RIGHT;
		this.place= place;
		mem = new ArrayList<Character>();
		mem.add('\0');//initial memory
		memPtr= 0;
		dStack= new LinkedList<Direction>();
		pStack= new LinkedList<Point>();
	}

	public CodePtr(Point place, Direction dir){
		//This constructor is left over from attempts at '&'
		this.dir= dir;
		this.place= place;
	}

	public void run(){
		for(;execute(code.get(place.y).charAt(place.x));place= moveCP());
	}

	private boolean execute(char inst){
		//ignore char and keep going
		if(!instChars.contains("" + inst)) return true;
		switch(inst){
		case '.'://print
			System.out.print(mem.get(memPtr));
			break;
		case ','://input
			try{
				int in;
				while((in= input.read()) == '\n' || in == '\r');//skip return chars
				mem.set(memPtr, (char)in);
			}catch(final IOException e){
				e.printStackTrace();
			}
			break;
		case '!'://skip
			place= moveCP();
			break;
		case '?'://conditional skip
			if(mem.get(memPtr) == '\0') place= moveCP();
			break;
		case '>'://move pointer right
			memPtr++;
			while(mem.size() <= memPtr){
				mem.add('\0');//add more memory cells if necessary
			}
			break;
		case '<'://move memory pointer left
			memPtr--;
			if(memPtr < 0){//no negative values
				System.err.println("memory pointer out of range (negative)");
				return false;//stop executing
			}
			break;
		case '/'://mirror
			switch(dir){
			case RIGHT:
				dir= Direction.UP;
				break;
			case DOWN:
				dir= Direction.LEFT;
				break;
			case LEFT:
				dir= Direction.DOWN;
				break;
			case UP:
				dir= Direction.RIGHT;
				break;
			default:
			}
			break;
		case '\\'://mirror
			switch(dir){
			case RIGHT:
				dir= Direction.DOWN;
				break;
			case DOWN:
				dir= Direction.RIGHT;
				break;
			case LEFT:
				dir= Direction.UP;
				break;
			case UP:
				dir= Direction.LEFT;
				break;
			default:
			}
			break;
		case '+'://inc
			mem.set(memPtr, (char)(mem.get(memPtr) + 1));
			break;
		case '-'://dec
			mem.set(memPtr, (char)(mem.get(memPtr) - 1));
			break;
		case '#'://pop
			if(dStack.size() > 0){
				Direction oldDir= dStack.pop();
				Point oldPlace= pStack.pop();
				place= oldPlace;
				dir= oldDir;
				return true;
			}
			return false;
		case '@'://push
			//protect the reference with a new object
			pStack.push(new Point(place));
			dStack.push(dir);
			place= moveCP();//skip
			break;
		case '%':
			mem.set(memPtr, (char)rand.nextInt(mem.get(memPtr) + 1));
			break;
		default:
		}
		return true;
	}

	private Point moveCP(){
		final Point retVal= new Point(place);
		switch(dir){
		case RIGHT:
			retVal.x++;
			break;
		case DOWN:
			retVal.y++;
			break;
		case LEFT:
			retVal.x--;
			break;
		case UP:
			retVal.y--;
			break;
		default:
		}
		if(retVal.x<0 || retVal.y<0 || 
				retVal.y >= code.size() || retVal.x >= code.get(0).length()){
			System.err.println("Code pointer has left the code space");
			System.exit(-1);
		}
		return retVal;
	}
	private enum Direction{
		UP,DOWN,LEFT,RIGHT;
	}
}
```

