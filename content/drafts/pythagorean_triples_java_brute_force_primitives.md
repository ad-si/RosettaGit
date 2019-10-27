+++
title = "Pythagorean triples/Java/Brute force primitives"
description = ""
date = 2011-12-20T20:45:58Z
aliases = []
[extra]
id = 10097
[taxonomies]
categories = []
tags = []
+++

{{works with|Java|1.5+}}
This version brute forces primitive triple candidates and then scales them to find the rest (under the perimeter limit of course). Since it only finds the primitives mathematically it can optimize its candidates based on some of the properties [[wp:Pythagorean_triple#Elementary_properties_of_primitive_Pythagorean_triples|here]] -- namely that a and b have opposite evenness, only one of a and b is divisible by 3, only one of a and b is divisible by 4, c is always odd, and that a<sup>2</sup> + b<sup>2</sup> must be a perfect square (which [[wp:Square_number#Properties|don't ever end in 2, 3, 7, or 8]]). After using those rules to eliminate candidates for a,b pairs, it checks that a and b are coprime. Since many a,b pair candidates have already been eliminated, this check actually speeds things up a little bit by letting the program skip some c loops. For a perimeter limit of 1000, it is about 5 times faster than [[Pythagorean triples#Java|the other brute force version]]. For a perimeter limit of 10000, it is about 17 times faster. It also marks the primitives.

It defines a <code>Triple</code> class which is comparable so it can be placed in a <code>TreeSet</code> for easy sorting and to remove duplicates (the GCD check should remove duplicates, but it's nice to make sure). It also can scale itself by an integer factor.

Note: this implementation also keeps all triples in memory. Be mindful of large perimeter limits.

```java5
import java.math.BigInteger;
import java.util.Set;
import java.util.TreeSet;

import static java.math.BigInteger.*;

public class PythTrip{
    private static final BigInteger TWO = BigInteger.valueOf(2),
            B3 = BigInteger.valueOf(3), B4 = BigInteger.valueOf(4),
            B7 = BigInteger.valueOf(7), B31 = BigInteger.valueOf(31),
            B127 = BigInteger.valueOf(127), B191 = BigInteger.valueOf(191);
    //change this to whatever perimeter limit you want;the RAM's the limit
    private static BigInteger LIMIT = BigInteger.valueOf(100);

    public static class Triple implements Comparable<Triple>{
        BigInteger a, b, c, peri;
        boolean prim;

        public Triple(BigInteger a, BigInteger b, BigInteger c, boolean prim){
            this.a = a;
            this.b = b;
            this.c = c;
            peri = a.add(b).add(c);
            this.prim = prim;
        }

        public Triple scale(long k){
            return new Triple(a.multiply(BigInteger.valueOf(k)), b
                    .multiply(BigInteger.valueOf(k)), c.multiply(BigInteger
                    .valueOf(k)), prim && k == 1);
        }

        @Override
        public boolean equals(Object obj){
            if(obj.getClass() != this.getClass())
                return false;
            Triple trip = (Triple) obj;
            return a.equals(trip.a) && b.equals(trip.b) && c.equals(trip.c);
        }

        @Override
        public int compareTo(Triple o){
            if(!a.equals(o.a))
                return a.compareTo(o.a);
            if(!b.equals(o.b))
                return b.compareTo(o.b);
            if(!c.equals(o.c))
                return c.compareTo(o.c);
            return 0;
        }

        public String toString(){
            return a + ", " + b + ", " + c + (prim ? " primitive" : "");
        }
    }

    private static Set<Triple> trips = new TreeSet<Triple>();

    public static void addAllScales(Triple trip){
        long k = 2;
        Triple tripCopy = trip.scale(k++);
        while(tripCopy.peri.compareTo(LIMIT) <= 0){
            trips.add(tripCopy);
            tripCopy = trip.scale(k++);
        }
    }

    public static void main(String[] args){
        long primCount = 0;
        long start = System.currentTimeMillis();

        BigInteger peri2 = LIMIT.divide(TWO), peri3 = LIMIT.divide(B3);

        for(BigInteger a = B3; a.compareTo(peri3) < 0; a = a.add(ONE)){
            BigInteger aa = a.multiply(a);
            boolean amod3 = a.mod(B3).equals(ZERO);
            boolean amod4 = a.mod(B4).equals(ZERO);

            //b is the opposite evenness of a so increment by 2
            for(BigInteger b = a.add(ONE); b.compareTo(peri2) < 0; b = b
                    .add(TWO)){
                //skip if both or neither of a and b are divisible by 3 and 4
                if(amod3 == b.mod(B3).equals(ZERO)
                        || amod4 == b.mod(B4).equals(ZERO))
                    continue;
                //if a^2+b^2 isn't a perfect square, don't even test for c's
                BigInteger aabb = aa.add(b.multiply(b));
                if((aabb.and(B7).intValue() != 1)
                        && (aabb.and(B31).intValue() != 4)
                        && (aabb.and(B127).intValue() != 16)
                        && (aabb.and(B191).intValue() != 0))
                    continue;
                if(!a.gcd(b).equals(ONE))
                    continue;
                BigInteger ab = a.add(b);

                // c is always odd for primitives so if b is odd start at b+2
                // otherwise b+1
                for(BigInteger c = b.add(b.testBit(0) ? ZERO : ONE); c
                        .compareTo(peri2) < 0; c = c.add(TWO)){

                    // if a+b+c > periLimit
                    if(ab.add(c).compareTo(LIMIT) > 0) break;

                    int compare = aabb.compareTo(c.multiply(c));
                    // if a^2 + b^2 != c^2
                    if(compare < 0){
                        break;
                    }else if(compare == 0){
                        Triple prim = new Triple(a, b, c, true);
                        if(trips.add(prim)){
                            primCount++;
                            addAllScales(prim);
                        }
                    }
                }
            }
        }
        for(Triple trip : trips){
            System.out.println(trip);
        }
        System.out.println("Runtime: " + (System.currentTimeMillis() - start));
        System.out.println("Up to a perimeter of " + LIMIT + ", there are "
                + trips.size() + " triples, of which " + primCount
                + " are primitive.");
    }
}
```

Output:

```txt
3, 4, 5 primitive
5, 12, 13 primitive
6, 8, 10
7, 24, 25 primitive
8, 15, 17 primitive
9, 12, 15
9, 40, 41 primitive
10, 24, 26
12, 16, 20
12, 35, 37 primitive
15, 20, 25
15, 36, 39
16, 30, 34
18, 24, 30
20, 21, 29 primitive
21, 28, 35
24, 32, 40
Up to a perimeter of 100, there are 17 triples, of which 7 are primitive.
```

