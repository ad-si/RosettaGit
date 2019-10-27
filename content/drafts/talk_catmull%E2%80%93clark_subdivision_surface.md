+++
title = "Talk:Catmull–Clark subdivision surface"
description = ""
date = 2019-09-24T19:14:21Z
aliases = []
[extra]
id = 5283
[taxonomies]
categories = []
tags = []
+++

== what's the input for the function? ==

What's the input for the function? Example input/output?

: typical 3D datas are formated like this: an array of vertex points first (x,y,z) 3D coordinates with type (float, float, float), they are indexed from 0 to (n - 1), then a list (or array) of faces, a triangle face is specified with 3 ints, and a quad face with 4 ints, the ints are the indexes in the first array. This data model is most often used because vertices belongs to several faces, so there are no duplication of the coordinates with this model.
: here are below a typical input and output for the Catmull-Clark subdivision surface algorythm, the input is a simple cube:

 input_points = [
   (-1.0,  1.0,  1.0);
   (-1.0, -1.0,  1.0);
   ( 1.0, -1.0,  1.0);
   ( 1.0,  1.0,  1.0);
   ( 1.0, -1.0, -1.0);
   ( 1.0,  1.0, -1.0);
   (-1.0, -1.0, -1.0);
   (-1.0,  1.0, -1.0);
 ]
 
 input_faces = [
   (0, 1, 2, 3);
   (3, 2, 4, 5);
   (5, 4, 6, 7);
   (7, 0, 3, 5);
   (7, 6, 1, 0);
   (6, 1, 2, 4);
 ]
 
 # below after one iteration:
 
 output_points = [
   (-0.555556,  0.555556,  0.555556);
   (-0.555556, -0.555556,  0.555556);
   ( 0.555556, -0.555556,  0.555556);
   ( 0.555556,  0.555556,  0.555556);
   ( 0.555556, -0.555556, -0.555556);
   ( 0.555556,  0.555556, -0.555556);
   (-0.555556, -0.555556, -0.555556);
   (-0.555556,  0.555556, -0.555556);
   ( 0.000000,  0.000000,  1.000000);
   (-0.750000,  0.000000,  0.750000);
   ( 0.000000, -0.750000,  0.750000);
   ( 0.750000,  0.000000,  0.750000);
   ( 0.000000,  0.750000,  0.750000);
   ( 1.000000,  0.000000,  0.000000);
   ( 0.750000, -0.750000,  0.000000);
   ( 0.750000,  0.000000, -0.750000);
   ( 0.750000,  0.750000,  0.000000);
   ( 0.000000,  0.000000, -1.000000);
   ( 0.000000, -0.750000, -0.750000);
   (-0.750000,  0.000000, -0.750000);
   ( 0.000000,  0.750000, -0.750000);
   ( 0.000000,  1.000000,  0.000000);
   (-0.750000,  0.750000,  0.000000);
   (-1.000000,  0.000000,  0.000000);
   (-0.750000, -0.750000,  0.000000);
   ( 0.000000, -1.000000,  0.000000);
 ]
 
 output_faces = [
   ( 0,  9,  8, 12);
   ( 1, 10,  8,  9);
   ( 2, 11,  8, 10);
   ( 3, 12,  8, 11);
   ( 3, 11, 13, 16);
   ( 2, 14, 13, 11);
   ( 4, 15, 13, 14);
   ( 5, 16, 13, 15);
   ( 5, 15, 17, 20);
   ( 4, 18, 17, 15);
   ( 6, 19, 17, 18);
   ( 7, 20, 17, 19);
   ( 7, 22, 21, 20);
   ( 0, 12, 21, 22);
   ( 3, 16, 21, 12);
   ( 5, 20, 21, 16);
   ( 7, 19, 23, 22);
   ( 6, 24, 23, 19);
   ( 1,  9, 23, 24);
   ( 0, 22, 23,  9);
   ( 6, 24, 25, 18);
   ( 1, 10, 25, 24);
   ( 2, 14, 25, 10);
   ( 4, 18, 25, 14);
 ]

== Algorithm Description Improvements ==

I've tried to improve the description of the algorithm (making it look more like math and less like computer code) but may have made a few bloopers along the way. A full mathematical version would be even better, except that I doubt that most people would immediately pick up the notation for a centroid and so would go horribly astray. Still, if someone can do some more work on it I'd be glad. Note also that neither the Wikipedia article nor this task are particularly good references; for example, the WP article only handles the hole-free mesh case. –[[User:Dkf|Donal Fellows]] 15:19, 18 January 2010 (UTC)

== Remove non-working OCaml example ==

Since nobody seems to be trying to fix the first OCaml example, perhaps it should be removed. Should we check with the original author first? - [[User:TobyK|TobyK]] 17:56, 17 August 2011 (UTC)

: Hi, I think candidate for deletion should rather be considering if an implementation is poor at the design point of view. Here the code produces wrong output but the error is probably a minor calculation error somewhere. There is also the Tcl example that produces wrong output on the border of the hole. Borders of holes should be smooth, and on the screenshot we can see that it's not. [[User:Blue Prawn|Blue Prawn]] 23:20, 17 August 2011 (UTC)

:: The Tcl solution is now fixed. It was a problem in the code to update point locations on the edge of the hole (which to be fair isn't actually discussed ''anywhere'' on the WP page or in the literature that I found with only a small amount of searching). The formula I picked seems to give nice-looking results. –[[User:Dkf|Donal Fellows]] 20:39, 7 November 2011 (UTC)
::: This current RC page explains how to handle this (which was found by examining results from blender.org). Could you check that what you have done is the same? [[User:Blue Prawn|Blue Prawn]] 01:04, 9 November 2011 (UTC)
:: In general though, only delete a solution if it is “considered harmful”, i.e., actively promoting bad practices not necessary to the solution of the task. Having a bug is not a sin, and there's no way to persuade people to fix things on any exact schedule. (You could try commenting on the original submitter's talk page; they ''might've'' configured email notifications.) Or try to learn enough OCaml to be able to fix it; the algorithm is complex enough that it remains itself the major challenge. –[[User:Dkf|Donal Fellows]] 20:39, 7 November 2011 (UTC)

== Should have another example geometry? ==

Looking at the examples, I can't really be sure that code handles meshes with holes properly.

Perhaps (a) we should have a task which renders these geometries, and (b) we should have a task example which has a hole in it? (Perhaps a cube with the top and bottom surfaces removed?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:59, 15 May 2015 (UTC)
