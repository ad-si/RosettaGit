+++
title = "Execute HQ9+/Java"
description = ""
date = 2010-02-06T14:29:50Z
aliases = []
[extra]
id = 4629
[taxonomies]
categories = []
tags = []
+++

{{implementation|HQ9+}}{{collection|RCHQ9+}}
This [[Java]] program accepts input from a file named by its first argument, or std in if no arguments are given.

Note: A [[Unicode]] trick could be done to make this source code a quine (similar to the one done in [[RCHQ9+/Tcl]], but also replacing the literal 99's with "100 - 1").

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.text.MessageFormat;
public class HQ9p{
  public static void main(String[] args) throws Exception{
     int acc = 0;
     String code = "";
     BufferedReader input;
     if(args.length > 0){
    	 input = new BufferedReader(new FileReader(args[0]));
     }else{
    	 input = new BufferedReader(new InputStreamReader(System.in));
     }
     while(input.ready()){
    	 code += input.readLine().toLowerCase();
     }
     
     for(char instr:code.toCharArray()){
    	 switch(instr){
    	 	case 'q': System.out.println(code); break;
    	 	case 'h': System.out.println("Hello, World!"); break;
    	 	case '9': printBottles(); break;
    	 	case '+': acc++; break;
    	 	default: //ignore other chars
    	 }
     }
  }

  public static void printBottles(){
     String byob = bottles(99);
     for (int x = 99; x > 0;) {
        System.out.println(byob + " on the wall");
        System.out.println(byob);
        System.out.println("Take one down, pass it around");
        byob = bottles(--x);
        System.out.println(byob + " on the wall\n");
     }
  }

  static String bottles(int n){
     return MessageFormat.format("{0,choice,0#No more bottles|" +
     		"1#One bottle|2#{0} bottles} of beer", n);
  }
}
```

