+++
title = "Talk:Haversine formula"
description = ""
date = 2014-02-11T07:55:58Z
aliases = []
[extra]
id = 12236
[taxonomies]
categories = []
tags = []
+++

== Different results ==

I find it interesting that there are two 'clusters' of results around 2886.4 and 2887.26
with very few 'significantly' differing ones-
Now: who is 'correct' here ??? and why are so many others 'wrong'??? --[[User:Walterpachl|Walterpachl]] 19:08, 16 August 2012 (UTC)

```txt

2886 km (1793 miles)                  
2886.326609413624                     
2886.327                              
2886.4 km                             
2886.44                               
2886.44 kilometers  (or 1793.55 miles)
2886.44444099822                      
2886.444442837984                     
2886.44444                            
2886.4                                
2887 km.                              
2887.2599 km                          
2887.2599506071097                    
2887.25995060711                      
2887.2599506071106                    
2887.2599506071106                    
2887.2599506071106                    
2887.259950607113                     
2887.25995060711                      
2887.26 km.                           
2887.26 km.                           
2887.260 km.                          
2887.2600 km                          
2887.26                               
2887.3 km (1794.1 mi.)                
2887.3 km                             
2889.68                               

```


:This might be due to the different values given for the rough radius of the Earth, in kilometers, for different examples. --[[User:Paddy3118|Paddy3118]] 19:22, 16 August 2012 (UTC)

-----

Some reasons probably are: 
* using a different (mean) radius of the earth
* using a derived radius from the earth's circumference
* use of difference precisions
* use of different formulas for trigonometric functions
* use of smaller numbers via radian (or degree) reduction/normalization.
-- [[User:Gerard Schildberger|Gerard Schildberger]] 19:25, 16 August 2012 (UTC)

Apparently it's the two different values used for the eartH's radius: 6372.8 and 6371.0, respectively-
--[[User:Walterpachl|Walterpachl]] 20:14, 16 August 2012 (UTC)

:Right, “6371.0” is the authalic radius based on/extracted from surface area, while “6372.8” is an approximation of the radius of the average circumference (i.e., the average “great-elliptic” or “great-circle radius”), where the boundaries are the meridian (≈ 6367.45 km) and the equator (≈ 6378.14 km).
:See ''[http://math.wikia.com/wiki/Ellipsoidal_quadratic_mean_radius Ellipsoidal quadratic mean radius]''. [[User:Kaimbridge|<span style="border:1px solid green;color:#e55b3c; padding:2px;background:#fde0bc">~<font face="courier new bold" class="title" title="Kaimbridge M. GoldChild">Kaimbridge</font>~</span>]] 17:44, 19 August 2012 (UTC)

::Thank you for the excellent explanation. I'd still want that explanation in the task description
::and would advise programmers to use one specific value in order to get comparable results 
::Maybe the one yielding a result of about 2887.26 km as this is the majority. --[[User:Walterpachl|Walterpachl]] 18:11, 19 August 2012 (UTC)

:::It's probably too late to ask for an update to the tasks. We know what causes the differences and the two results that are obtained from using the two radii, and have noted it here. --[[User:Paddy3118|Paddy3118]] 20:57, 19 August 2012 (UTC)


::::It's never too late:-) I suggest to MENTION these radii in the task and RECOMMEND one for future implementers
::::Was it you who said that it's difficult to create watertight task descriptions:-)
::::Thanks anyway --[[User:Walterpachl|Walterpachl]] 05:07, 20 August 2012 (UTC)
:::::''Recommend a radius for future implementors''. Mention both and then recommend one sounds good as it would leave the examples already there alone or allow them to be updated. Or would we want to force all the examples to use the one radius? (Is it that much of a problem? --[[User:Paddy3118|Paddy3118]] 07:26, 20 August 2012 (UTC)
:I have a sneaking suspicion that the fortran code c = 2*atan2(sqrt(a),sqrt(1-a)) is wrong. I can't prove it. Does anyone have a Fortran compiler handy?

I've change the recommended radius to the mean earth radius.  This choice minimizes the RMS relative error and is consistent with the choice which conserves the volume and the area of the earth (in the limit of small flattening).  The previous recommendation was the "quadratic mean radius".  Unfortunately, its derivation contained a flaw in the way azimuths were sampled, so it does not minimize any reasonable error metric.  For an extended discussion see this [https://en.wikipedia.org/wiki/Talk:Great-circle_distance/Archive_1 archived talk page for Great-circle distances] on Wikipedia. [[User:Cffk|cffk]] ([[User talk:Cffk|talk]]) 22:48, 10 February 2014 (UTC)

:With 53 examples, it is too late to make most radical changes. Try appending a recommendation instead. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:55, 11 February 2014 (UTC)
