+++
title = "Example:Hough transform/MATLAB"
description = ""
date = 2013-03-30T15:34:10Z
aliases = []
[extra]
id = 8043
[taxonomies]
categories = []
tags = []
+++

{{Programming-example-page|Hough transform|language=MATLAB}}

This solution takes an image and the theta resolution as inputs. The image itself must be a 2-D boolean array. This array is constructed such that all of the pixels on an edge have the value "true." This can be done for a normal image using an "edge finding" algorithm to preprocess the image. In the case of the example image the pentagon "edges" are black pixels. So when the image is imported into MATLAB simply say any pixel colored black is true. The syntax is usually, cdata < 255. Where the vale 255 represents white and 0 represents black.


```MATLAB
function [rho,theta,houghSpace] = houghTransform(theImage,thetaSampleFrequency)

    %Define the hough space
    theImage = flipud(theImage);
    [width,height] = size(theImage);

    rhoLimit = norm([width height]);
    rho = (-rhoLimit:1:rhoLimit);
    theta = (0:thetaSampleFrequency:pi);

    numThetas = numel(theta);
    houghSpace = zeros(numel(rho),numThetas);

    %Find the "edge" pixels
    [xIndicies,yIndicies] = find(theImage);

    %Preallocate space for the accumulator array
    numEdgePixels = numel(xIndicies);
    accumulator = zeros(numEdgePixels,numThetas);

    %Preallocate cosine and sine calculations to increase speed. In
    %addition to precallculating sine and cosine we are also multiplying
    %them by the proper pixel weights such that the rows will be indexed by
    %the pixel number and the columns will be indexed by the thetas.
    %Example: cosine(3,:) is 2*cosine(0 to pi)
    %         cosine(:,1) is (0 to width of image)*cosine(0)
    cosine = (0:width-1)'*cos(theta); %Matrix Outerproduct
    sine = (0:height-1)'*sin(theta); %Matrix Outerproduct

    accumulator((1:numEdgePixels),:) = cosine(xIndicies,:) + sine(yIndicies,:);

    %Scan over the thetas and bin the rhos
    for i = (1:numThetas)
        houghSpace(:,i) = hist(accumulator(:,i),rho);
    end

    pcolor(theta,rho,houghSpace);
    shading flat;
    title('Hough Transform');
    xlabel('Theta (radians)');
    ylabel('Rho (pixels)');
    colormap('gray');

end
```


Sample Usage:

```matlab
>
 uiopen('C:\Documents and Settings\owner\Desktop\Chris\MATLAB\RosettaCode\180px-Pentagon.png',1)
>> houghTransform(cdata(:,:,1)<255,1/200); %The image from uiopen is stored in cdata. The reason why the image is cdata<255 is because the "edge" pixels are black.
```

[[Image:HoughTransformHex.png|thumb|left|360x200px|Image produced by MATLAB implementation of the Hough transform when applied to the sample pentagon image.]]
<br style="clear:both" />
