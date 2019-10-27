+++
title = "FizzBuzz/Java"
description = ""
date = 2015-08-20T07:08:23Z
aliases = []
[extra]
id = 18256
[taxonomies]
categories = []
tags = []
+++

{{collection|FizzBuzz}}


### If/else ladder


```java
public class FizzBuzz{
	public static void main(String[] args){
		for(int i= 1; i <= 100; i++){
			if(i % 15 == 0){
				System.out.println("FizzBuzz");
			}else if(i % 3 == 0){
				System.out.println("Fizz");
			}else if(i % 5 == 0){
				System.out.println("Buzz");
			}else{
				System.out.println(i);
			}
		}
	}
}
```



### Concatenation


```java
public class FizzBuzz{
	public static void main(String[] args){
		for(int i= 1; i <= 100; i++){
			String output = "";
			if(i % 3 == 0) output += "Fizz";
			if(i % 5 == 0) output += "Buzz";
			if(output.equals("")) output += i;
			System.out.println(output);
		}
	}
}
```


### Ternary operator


```java
public class FizzBuzz{
	public static void main(String[] args){
		for(int i= 1; i <= 100; i++){
			System.out.println(i % 15 != 0 ? i % 5 != 0 ? i % 3 != 0 ? 
			i : "Fizz" : "Buzz" : "FizzBuzz");
		}
	}
}
```


### Recursive


```java
public String fizzBuzz(int n){
  String s = "";
  if (n == 0)
    return s;
  if((n % 5) == 0)
    s = "Buzz" + s;
  if((n % 3) == 0)
    s = "Fizz" + s;
  if (s.equals(""))
    s = n + "";   
  return fizzBuzz(n-1) +  s;
}
```


### Alternative Recursive


```java
public String fizzBuzz(int n){
  return (n>0) ? fizzBuzz(n-1) + 
    (n % 15 != 0? n % 5 != 0? n % 3 != 0? (n+"") :"Fizz" : "Buzz" : "FizzBuzz") 
               : "";
}
```


###  Using an array 


```java
class FizzBuzz {
  public static void main( String [] args ) {
    for( int i = 1 ; i <= 100 ; i++ ) {
      System.out.println( new String[]{ i+"", "Fizz", "Buzz", "FizzBuzz" }[ ( i%3==0?1:0 ) + ( i%5==0?2:0 ) ]);
    }
  }
}
```



### Lambda with Arrays


```java
class FizzBuzz {
  public static void main( String [] args ) {
    int [] x = new int [100];
    Arrays.setAll(x, j -> j++);
    Arrays.stream(x).forEach(i -> {
        if(i == 0) return;
        String output = "";
        if(i % 3 == 0) output += "Fizz";
        if(i % 5 == 0) output += "Buzz";
        if(output.equals("")) output += i;
        System.out.println(output);
    });
  }
}
```



### Lambda with Streams


```java
package fizzbuzz;

import static java.util.stream.IntStream.rangeClosed;

public interface FizzBuzz {
  public static void main(String... arguments) {
    rangeClosed(1, 100)
      .mapToObj(i -> {
        if (i % (3 * 5) == 0) {
          return "FizzBuzz";
        } else if (i % 3 == 0) {
          return "Fizz";
        } else if (i % 5 == 0) {
          return "Buzz";
        } else {
          return Integer.toString(i);
        }
      })
      .forEach(System.out::println)
    ;
  }
}
```



###  OO style 


```java
public class FizzBuzz {

    public static void main(String[] args) {
        Sound sound = new Sound(3, "Fizz", new Sound(5, "Buzz"));
        for (int i = 1; i <= 100; i++) {
            System.out.println(sound.generate(i));
        }
    }

    private static class Sound {
        private final int trigger;
        private final String onomatopoeia;
        private final Sound next;

        public Sound(int trigger, String onomatopoeia, Sound next) {
            this.trigger = trigger;
            this.onomatopoeia = onomatopoeia;
            this.next = next;
        }

        public Sound(int trigger, String sound) {
            this(trigger, sound, null);
        }

        public String generate(int i) {
            StringBuilder sb = new StringBuilder();
            generate(sb, i);
            return sb.length() == 0 ? String.valueOf(i) : sb.toString();
        }
        
        private void generate(StringBuilder sb, int i) {
            if (i % trigger == 0)
                sb.append(onomatopoeia);
            if (next != null) 
                next.generate(sb, i);
        }

    }

}
```

