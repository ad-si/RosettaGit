+++
title = "Talk:Perceptron"
description = ""
date = 2017-02-25T19:00:17Z
aliases = []
[extra]
id = 21011
[taxonomies]
categories = []
tags = []
+++

== How long does that java implementation need? ==

Looking at the current java implementation, I see, in the paint event:


```java
        train(training[count].inputs, training[count].answer);
        count = (count + 1) % training.length;
 
        g.setStroke(new BasicStroke(1));
        g.setColor(Color.black);
        for (int i = 0; i < count; i++) {
```


So, I believe count is initially zero, and it looks like each time through the paint event there's training against one element of the training set. And, it's got a timer doing a repaint every 30 milliseconds. So it's a bit over a minute to do one pass against the training set of 2000 elements, and then it will keep training indefinitely.

It would be helpful to know how long the java implementation had to "cook" before that screenshot was taken... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:10, 16 July 2016 (UTC)
: Not very long, maybe a few minutes, I can't remember exactly, but it wasn't very long. It stabilizes after about half a minute on my system, but I almost never get a perfect split (I believe the text at natureofcode also says that), so I got a bit lucky with the screenshot. I tried making it run longer but it doesn't seem to improve. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 07:13, 16 July 2016 (UTC)

== Is the line on Java correct? ==

The line drawn here is from (0,0) to (x,f(x)). Should it not be from (0,f(0)) to (x, f(x))? This is the line we're partitioning acress? Surely?


```java

int x = getWidth();
int y = (int) f(x);
//. [...]
// g.drawLine(0, 0, x, y);
g.drawLine(0, (int)f(0), x, y);
 
```


--[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 16:59, 16 January 2017 (UTC)

== Tasks ==

Excellent: I was looking for some machine-learning style tasks, and found this one!

As it's still in draft, could I suggest some additional tasks to be done?  I also suggest the image output be moved to 'extra credit': image output is difficult for languages without a natural gui library, and there also seem to be problems uploading images to the site, so results are hard to show.  (Although I do like the Java version's dynamic graphical output.)

As a core set of tasks, how about simply showing the perceptron being trained and working:

# Create a set of training data of at least 5000 random points, classifying the points with a line of your choice.
# Train a perceptron from the training data.
# Show the final perceptron's weights
# Create a new test set of at least 1000 random points, and show the percentage correct achieved by the final perceptron.


As 'extra credit' tasks:

# Draw an image showing the target line and the decisions made by the perceptron on the test set.
# Show how performance on a fixed test set varies with an increasing range of training data, say 1000 to 20000 points in steps of 1000.


I shall add a Scheme example illustrating these core tasks and the second extra one.

-- [[User:Peter]] 17:15, 25 February 2017 (UTC)

: Do you feel like [[Rosetta_Code:Add_a_Task|drafting]] one or more of these tasks? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:59, 25 February 2017 (UTC)
