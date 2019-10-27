+++
title = "Library/C Runtime/ranged rand"
description = ""
date = 2012-12-16T18:24:11Z
aliases = []
[extra]
id = 8832
[taxonomies]
categories = []
tags = []
+++

#include <cstdlib>
    
    ...
    
    int rangedRand(int min, int max)
    {
        return rand() % (max - min + 1) + min;
    }
