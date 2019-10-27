+++
title = "Help:Orphaned code"
description = ""
date = 2014-04-27T07:18:42Z
aliases = []
[extra]
id = 1837
[taxonomies]
categories = []
tags = []
+++

Sometimes, articles get rearranged, or even repurposed, and code no longer fits in.  This is a place where such code may be stored, rather than deleted outright.

==[[C++]]==
===for_each ===
'''Compiler:''' [[g++]] 4.1.1

  #include <iostream>  // std::cout, std::endl
  #include <vector>    // std::vector
  #include <algorithm> // std::for_each
  
  struct sum
  {
    int _sum;                                    
    sum() : _sum(0) {};                         // Initialize sum with 0;
    void operator() (int a) { _sum += a; }      // this function will be called for every element
  };
  
  int main()
  {
    std::vector<int> v;
    v.push_back(10);
    v.push_back(23);
    v.push_back(34);
  
    /* Note that for_each gets a fresh instance of sum passed,
     * applies every element beginning with *v.begin() up to,
     * but not including v.end() to the function object
     * and returns a copy of it.
     */
  
    sum the_sum = std::for_each(v.begin(), v.end(), sum());
  
    std::cout << "The sum is " << the_sum._sum << std::endl;
    return 0;
  }
