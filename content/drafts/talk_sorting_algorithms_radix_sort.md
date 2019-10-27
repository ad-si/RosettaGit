+++
title = "Talk:Sorting algorithms/Radix sort"
description = ""
date = 2014-02-01T08:42:34Z
aliases = []
[extra]
id = 9172
[taxonomies]
categories = []
tags = []
+++

==Negatives==
Beware negative number handling! See [http://codepad.org/MLi6H7Y1 Wiki's python demo]. [[User:Dingowolf|dingowolf]] 13:25, 19 January 2011 (UTC) 
: An interesting problem; the easiest way to handle it seems to me to be to double the number of bins and put negative values in the first half and positive in the second. Or at least it produces correct results when I implemented it in the Tcl solution. (I suspect that the original algorithm simply didn't implement them, or sorted by printed digit instead of logical digit.) –[[User:Dkf|Donal Fellows]] 13:22, 19 January 2011 (UTC)
:: The easiest way to handle negative numbers might be to find the minimum value in the list, subtract it from every item in the unsorted list and add it to every item in the sorted list.  This approach is modular and can wrap any "non-negative integers only" implementation, and work well in a variety of circumstances.  That said the "double the bins" approach might have an efficiency advantage when the the absolute value of the maximum equals the absolute value of the minimum.  --[[User:Rdm|Rdm]] 16:13, 19 January 2011 (UTC)
::: It was a smaller change to the code I already had working for the positive case. :-) –[[User:Dkf|Donal Fellows]] 16:42, 19 January 2011 (UTC)

::: Yuppers, the negative integers were a small annoyance, all right (concerning the REXX example). -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:03, 11 June 2012 (UTC)

==C code==
in the C code for radix sort, it seems to me that the condition ll < to after the "while (1)" loop is always fulfilled, and can thus be removed. Indeed in the "while (1)" loop we always have ll <= rr, thus since rr decreases ll cannot exceed the initial value of rr, which is to - 1.
[[User:Paul Zimmermann]] 13:09, 30 October 2012

==Java==

I made a shorter/simpler implementation of the java example (it also handles negatives)


```java
public static int[] sort(int[] old) {
			
	for(int shift = Integer.SIZE-1; shift > -1; shift--) { //Loop for every bit in the integers
		
		int[] tmp = new int[old.length]; //the array to put the partially sorted array into
		int j= 0;  //The number of 0s
		int i;  //Iterator

	
		for(i = 0; i < old.length; i++){  //Move the 0s to the new array, and the 1s to the old one
			
			boolean move = old[i] << shift >= 0;  //If there is a 1 in the bit we are testing, the number will be negative  
			
			if(shift == 0 ? !move : move) {  //If this is the last bit, negative numbers are actually lower
				
				tmp[j] = old[i];
				j++;
				
			} else {  //It's a 1, so stick it in the old array for now
				
				old[i-j] = old[i]; 
				
			}
			
		}
		
		for(i = j; i < tmp.length; i++) {  //Copy over the 1s from the old array
			
			tmp[i] = old[i-j];	
			
		}
		
		old = tmp;  //And now the tmp array gets switched for another round of sorting
		
	}
			
	return old;
	
}
```


[[User:Forty-Bot|Forty-Bot]] ([[User talk:Forty-Bot|talk]])
