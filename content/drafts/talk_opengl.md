+++
title = "Talk:OpenGL"
description = ""
date = 2019-01-23T16:41:19Z
aliases = []
[extra]
id = 2155
[taxonomies]
categories = []
tags = []
+++

This task really needs local hosting of the image in question.  I'll work on enabling that sometime later today.--[[User:Short Circuit|Short Circuit]] 03:19, 7 October 2007 (MDT)
:Took me longer than expected, but it is done.  See the resident bureaucrats if you need an image uploaded. --[[User:Short Circuit|Short Circuit]] 22:25, 9 October 2007 (MDT)

Also, this task should be [[Help:Generic|genericized]], to allow the use of either OpenGL, Direct3D, or any other 3D drawing API.  I'd like to see 3D graphics get their own section under [[:Category:Solutions by Programming Task]], and this page get renamed to "Create a poly" or some such. --[[User:Short Circuit|Short Circuit]] 12:21, 8 October 2007 (MDT)

Another item: The C example appears to create a managed window, which goes slightly beyond the bounds of the task.  Considering that such steps may be necessary for some languages, it should be mentioned in the example preamble. --[[User:Short Circuit|Short Circuit]] 12:21, 8 October 2007 (MDT)


### Go entry broken

It seems "github.com/mewmew/glfw/win" has not been updated in 5 years, matching when the Go sample was last modified.
My suspicion is that glfw has been updated meanwhile and now split into 3.0/3.1/3.2 or something like that.
I tried replacing "github.com/go-gl/glfw3" with "github.com/go-gl/glfw/v3.2/glfw" in error.go, which seemed 
to help at least by (apparently) requiring the same change in events.go then win.go, but that was as far as
I got before it all collapsed with a plethora of new errors. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 16:47, 22 January 2019 (UTC)

:Hi Pete. Although I didn't write the original, I've managed to fix it (at least on my Ubuntu 16.04 box) by using the newer 'GLFW 3.2 for Go' library. When I have time, I'll see if it works on Windows 10 as well.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 20:20, 22 January 2019 (UTC)

::Hi. Tested on Windows 10: failed with "unable to initialize VERSION_1_2", tracked down to the err from gl.Init().
::In case it helps, it opens C:\Windows\System32\opengl32.dll and glu32.dll, both version 10.0.17134.1 (18/04/2018).
::There was no error from glfw.Init(), and if I comment out the third&last check(err) line, it runs fine. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 23:31, 22 January 2019 (UTC)

:I've just tried it on Windows 10 myself but with exactly the same results as you obtained.

:My versions of opengl32.dll and glu32.dll are a bit earlier than yours - 10.0.16299.15 (29/09/17).

:I've tracked down the error to this C function which is called (via cgo) by gl.Init() in gl21.go:


```c
int init_VERSION_2_1() {
    ptrglUniformMatrix2x3fv = goglGetProcAddress("glUniformMatrix2x3fv");
    if(ptrglUniformMatrix2x3fv == NULL) return 1;
    ptrglUniformMatrix3x2fv = goglGetProcAddress("glUniformMatrix3x2fv");
    if(ptrglUniformMatrix3x2fv == NULL) return 1;
    ptrglUniformMatrix2x4fv = goglGetProcAddress("glUniformMatrix2x4fv");
    if(ptrglUniformMatrix2x4fv == NULL) return 1;
    ptrglUniformMatrix4x2fv = goglGetProcAddress("glUniformMatrix4x2fv");
    if(ptrglUniformMatrix4x2fv == NULL) return 1;
    ptrglUniformMatrix3x4fv = goglGetProcAddress("glUniformMatrix3x4fv");
    if(ptrglUniformMatrix3x4fv == NULL) return 1;
    ptrglUniformMatrix4x3fv = goglGetProcAddress("glUniformMatrix4x3fv");
    if(ptrglUniformMatrix4x3fv == NULL) return 1;
    return 0;
}
```


:So it appears that one (or more) of these tests is producing a null pointer though - as it works fine when the error returned by :gl.Init() is ignored - that this doesn't matter as far as this particular application is concerned.

:I've tried various things including updating the graphics card driver but to no avail so, unless you've any other ideas, I think for now I'll just note that it ''may'' only work on Windows 10 if you ignore the initialization error.--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 11:15, 23 January 2019 (UTC)

::plenty good enough for me, thanks. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 16:40, 23 January 2019 (UTC)
