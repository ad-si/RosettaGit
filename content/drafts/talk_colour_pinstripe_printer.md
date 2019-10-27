+++
title = "Talk:Colour pinstripe/Printer"
description = ""
date = 2011-05-23T18:19:16Z
aliases = []
[extra]
id = 9760
[taxonomies]
categories = []
tags = []
+++

We've got a 2400 dpi color printer; ''no way'' am I going to do 1 pixel wide color stripes on it, as it'll just look like mush. Perhaps the task needs some rewording to take into account a greater diversity of hardware? –[[User:Dkf|Donal Fellows]] 14:39, 23 May 2011 (UTC)

The purpose is demonstrate pixe by pixel control of the printer by the production of a test pattern. The 1 pixel wide stripes may mush on some printers. However, as you go down the page, the stripes get wider. This would be considered normal, and can be used as a reference point for
comparing printers.

[[User:Markhobley|Markhobley]] 15:01, 23 May 2011 (UTC)

Yes, after an inch the pinstripe is 2 pixels wide, after a foot, it's 12 pixels wide, after 10 feet, it's 120 pixels wide.  But I am not quite sure what this means for an HP Designjet with a full roll of paper.  --[[User:Rdm|Rdm]] 15:20, 23 May 2011 (UTC)
: Perhaps a reasonable limitation would be "no fewer than three color cycles, and then no greater than one square"? (where square would be the largest equal-logical-width, equal-logical-height space--a 1ftx250ft roll would produce a 1ft square.) Go three cycles, and then continue up to 1-square.  Also...I wonder what Windows' GetDeviceCaps reports for that printer for HORZSIZE, VERTSIZE, HORZRES and VERTRES --[[User:Short Circuit|Michael Mol]] 15:55, 23 May 2011 (UTC)

:: If you have some simple code I can build under VS2010, I could run it and show you the output.  But the [http://support.microsoft.com/kb/193943 example code I saw for GetDeviceCaps] was too much of a side track for me.  --[[User:Rdm|Rdm]] 17:57, 23 May 2011 (UTC)
::: Sure. Using VS2010 here at work. Don't know exactly how soon I'll be able to write it, but I'll look into it. --[[User:Short Circuit|Michael Mol]] 18:19, 23 May 2011 (UTC)

The task is really to demonstrate the coding. You could always adjust the run length and step distances for a specific implementation in live code (Maybe set variables at the top of the code). I don't think we need to worry about that for the purpose of this task. As long as the
task demonstrates the fine control of the printer, a reader would be able to wield the techniques demonstrated to achieve their specific goals.

I think the term square is more confusing, because a square could be any size, whereas a pixel means the smallest dot (possibly a tridot) that the printer can produce.


[[User:Markhobley|Markhobley]] 15:57, 23 May 2011 (UTC)
: Ok, what do you mean by 'fine control of the printer'? I'm getting the feeling you've got an idea of what you're looking to demonstrate, but it's not coming across clearly. --[[User:Short Circuit|Michael Mol]] 17:24, 23 May 2011 (UTC)

I know how to do most of this in C (or C++) on Windows (via [[GDI]] and a syscall) for ''raster'' printers, with the exception of [http://msdn.microsoft.com/en-us/library/dd144877%28VS.85%29.aspx querying the printer driver] to get awareness of the ink colors available. (For example, to my knowledge, most home HP inkjet printers support a five-color model, if the right cartridges are used. I'm not aware of these capabilities being made available to userland applications, though; I'm fairly sure they're used as a dither basis in either the printer driver or the printer's internal processor.). Also, this task is specifically plausible only for raster printer devices. I would suggest that the task be renamed to [[Colour pinstripe/Raster]]. Since the task as-written only supports devices which allow bypassing of driver and internal processing, it may also be sensible to specify an explicit color order (such as CMYK(CM|B)(MY|R)(YC|G), or else each example would have to be specific to a particular printer control language. That said, comparison of PostScript vs PCL is a good thing, but it'd be preferable for it to not be the ''only'' thing. --[[User:Short Circuit|Michael Mol]] 15:55, 23 May 2011 (UTC)

I don't think we necessarily need to bypass the driver. It may be possible for the application to tell the driver to print the pinstripe, and the driver does the printing. Likewise, the code could tell the driver what colour it wants, and the driver can do the necessary adjustments
using the inks available in the device. (It would be reasonable to expect black, red, blue, green, magenta, cyan and yellow, and white).

[[User:Markhobley|Markhobley]] 16:04, 23 May 2011 (UTC)
: I've never used a printer that had white ink. Or read, green or blue ink, for that matter. Printers generally operate on pigments, which operate on a light-subtractive model, not a light-additive model. As far as whether or not it's possible to tell the driver to print the pinstripe--if it is, it's not documented for how to do that on Windows, and that suggests to me that it would then be a vendor-specific thing (The generic interface on Windows for querying what drivers are capable of is GetDeviceCaps. And, actually, I'm failing to find functions for drawing in CMYK; it looks like it's all done in RGB, and the printer driver does the necessary translation), if supported at all. This feels a bit like a terminology blockage, though; by "inks available in the device", do you mean, "simple colors the device can represent"? --[[User:Short Circuit|Michael Mol]] 17:24, 23 May 2011 (UTC)

I assumed white paper for this task. For a white pinstripe, do not apply any ink to the stripe. Red = Yellow + Magenta, Green = Cyan + Yellow, Blue = Cyan + Magenta. If the printer does not have cyan, magenta, yellow, then for this task, then it doesn't matter what inks the printer actually uses, just make an approximation of the basic colours, eg show how to request each of the colours from the driver and let the driver do the mixing.

[[User:Markhobley|Markhobley]] 17:41, 23 May 2011 (UTC)
