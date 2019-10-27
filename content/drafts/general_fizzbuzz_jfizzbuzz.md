+++
title = "General FizzBuzz/jFizzBuzz"
description = ""
date = 2019-06-11T19:25:22Z
aliases = []
[extra]
id = 21097
[taxonomies]
categories = []
tags = []
+++


```java
/**
 * @file GeneralFizzBuzz.java
 */

import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Scanner;


/**
 * This class follows the "God class" pattern.
 * See {@link #main(String[])} for example usage.
 */
class FizzBuzzer
implements
    Iterable<FizzBuzzer.Entry>,
    Iterator<FizzBuzzer.Entry>
{
    private static final long DEFAULT_START_NUM = 1;
    private static final long DEFAULT_MAX_NUM = Long.MAX_VALUE;

    public class Entry {

        public final long num;
        public final List<String> names;

        private Entry(Long num, List<String> names) {
            this.num = num;
            this.names = names;
        }
    }

    private Map<Integer, String> modToName = new HashMap<>();

    private long currNum = DEFAULT_START_NUM;
    private long maxNum  = DEFAULT_MAX_NUM;
    private Map<Long, List<Integer>> schedule = new HashMap<>();

    public long getCurrNum() { return currNum; }
    public void setCurrNum(long currNum) { this.currNum = currNum; }

    public long getMaxNum() { return maxNum; }
    public void setMaxNum(long maxNum) { this.maxNum = maxNum; }


    private List<Integer> scheduledAt(long num) {
        return schedule.computeIfAbsent(num, num_ -> new ArrayList<>());
    }

    private void scheduleNearest(Integer mod) {

        long nearestNum = currNum + (mod - currNum % mod);

        scheduledAt(nearestNum).add(mod);
    }

    public boolean add(Integer mod, String name) {

        if (modToName.containsKey(mod)) {
            return false;
        }

        modToName.put(mod, name);
        scheduleNearest(mod);

        return true;
    }

    public void addAll(Map<Integer, String> modToName) {

        for (Map.Entry<Integer, String> modAndName : modToName.entrySet()) {
            add(modAndName.getKey(), modAndName.getValue());
        }
    }


    @Override
    public Entry next() {

        List<Integer> currMods = scheduledAt(currNum);

        List<String> currNames = new ArrayList<>();

        for (Integer m : currMods) {

            String name = modToName.get(m);
            currNames.add(name);

            long newNum = currNum + m;
            scheduledAt(newNum).add(m);
        }

        Entry result = new Entry(currNum, currNames);

        currNum++;

        return result;
    }

    @Override
    public void remove() {
        schedule.remove(currNum - 1);
    }

    @Override
    public boolean hasNext() {
        return (maxNum < 0) || (currNum <= maxNum);
    }

    @Override
    public Iterator<Entry> iterator() {
        return this;
    }


    public static FizzBuzzer readFrom(InputStream in) {

        FizzBuzzer fizzBuzzer = new FizzBuzzer();

        try (Scanner scanner = new Scanner(in)) {

            long maxNum = scanner.nextLong();
            fizzBuzzer.setMaxNum(maxNum);

            while (scanner.hasNext()) {

                Integer mod = scanner.nextInt();
                scanner.skip("[\t ]*");
                String name = scanner.nextLine();

                fizzBuzzer.add(mod, name);
            }
        }

        return fizzBuzzer;
    }

    public void printTo(OutputStream out) {

        try (PrintWriter writer = new PrintWriter(out)) {

            for (FizzBuzzer.Entry e : this) {

                String strNames = String.join(" ", e.names);
                String line = String.format("%d %s", e.num, strNames);

                writer.println(line);
                writer.flush();

                remove();
            }
        }
    }

    @Override
    public String toString() {
        return String.format("%s; current %d; max %d",
                             modToName, currNum, maxNum);
    }
}

class GeneralFizzBuzz {

    public static void main(String[] args) {

        try {
            FizzBuzzer.readFrom(System.in).printTo(System.out);
        }

        catch (NoSuchElementException | IllegalStateException e) {

            System.err.format("Error: %s\n", e.toString());
            System.exit(1);
        }
    }
}
```

