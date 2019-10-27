+++
title = "Talk:Top rank per group"
description = ""
date = 2010-05-21T13:51:15Z
aliases = []
[extra]
id = 3206
[taxonomies]
categories = []
tags = []
+++

==Task Description is just about missing==
What does the task require one to do?
What group?
What do you mean by ranker? --[[User:Paddy3118|Paddy3118]] 12:46, 5 December 2008 (UTC)
: It looks like the SMMEQL example was the original, definitive example on the page, so a new task description should be written with that as a basis.  I'll get to it tomorrow on my lunch break, if nobody beats me to it. --[[User:Short Circuit|Short Circuit]] 07:41, 10 February 2009 (UTC)

I just changed the task description to show "department" instead of "group" because I thought it was unclear originally. I was going to explain it in my edit comment, but I tapped enter by accident when putting in a quote mark. --[[User:Mwn3d|Mwn3d]] 20:57, 25 March 2010 (UTC)

==J==
The task description does not seem to qualify the output if the sorted values within one department were as follows:
 9 8 8 7 7 7 7 7 6 6 6 5 5 4
--[[User:TBH|TBH]] 05:22, 4 December 2008 (UTC)

: I'm going to be away the next couple weeks, but here's an approach in J.  
: Using the following input:
    Employees=: (<;.1~ (#{.,~@:1:);+./@:(;: E.S:0 ])@:{.) ];._2 noun define
 EMP_NAME        EMP_ID HIRE_DATE SALARY DEPT EXEMPT INTERESTS 
 Tyler Bennett   E10297 19770601  32000  D101 Y                
 John Rappl      E21437 19870715  47000  D050 Y               1
 George Woltman  E00127 19820807  53500  D101 Y               2
 Adam Smith      E63535 19880115  18000  D202 N                
 Claire Buckman  E39876 19851123  27800  D202 Y                
 David McClellan E04242 19820727  41500  D101 Y               3
 Rich Holcomb    E01234 19830601  49500  D202 Y                
 Nathan Adams    E41298 19880215  21900  D050 N                
 Richard Potter  E43128 19860412  15900  D101 N                
 David Motsinger E27002 19850505  19250  D202 N                
 Tim Sampair     E03033 19871202  27000  D101 Y                
 Kim Arlich      E10001 19850730  57000  D190 Y                
 Timothy Grove   E16398 19850121  29900  D190 Y                
 )
: The top earners in each dept are:<code>
    ('`',,;:^:_1:N=:{.Employees)  =: , (_&{"1)`<nowiki>''</nowiki> ([^:(_ -: ])L:0)"0 _~ i.# E=:{:Employees
    N , (<@:>"1@:|:@:((6 <. #) {. ] \: SALARY)/.~ DEPT) |: <"1&> E
: --[[User:DanBron|DanBron]] 15:13, 4 December 2008 (UTC)
C++0X is not yet ratified, but maybe an implementation could be given anyway :

```c

#include <string>
#include <iostream>
#include <tuple>
#include <iterator>
#include <vector>
#include <algorithm>
int main(int argc, char* argv[]) {
    typedef std::tuple<std::string , std::string , std::size_t, std::string > employee_t;
    std::vector<employee_t> v={employee_t("Tyler Bennett", "E10297", 32000, "D101"),
                               employee_t("John Rappl", "E21437", 47000, "D050"),
                               employee_t("George Woltman", "E21437", 53500, "D101"),
                               employee_t("Adam Smith", "E21437", 18000, "D202"),
                               employee_t("Claire Buckman", "E39876", 27800, "D202"),
                               employee_t("David McClellan", "E04242", 41500, "D101"),
                               employee_t("Rich Holcomb", "E01234", 49500, "D202"),
                               employee_t("Nathan Adams", "E41298", 21900, "D050"),
                               employee_t("Richard Potter", "E43128", 15900, "D101"),
                               employee_t("David Motsinger", "E27002", 19250, "D202"),
                               employee_t("Tim Sampair", "E03033", 27000, "D101"),
                               employee_t("Kim Arlich", "E10001", 57000, "D190"),
                               employee_t("Timothy Grove", "E16398", 29900, "D190")};
    std::sort(v.begin(),v.end(),[](employee_t const& a, employee_t const& b){ return std::get<2>(a)>std::get<2>(b);});
    std::sort(v.begin(),v.end(),[](employee_t const& a, employee_t const& b){ return std::get<3>(a)<std::get<3>(b);});
    std::size_t const to_display(3);
    std::size_t displayed(0);
    std::string const* last_dept(0);
    std::for_each(v.begin(), v.end(),[&](employee_t const&e){
            std::string const& current_dep(std::get<3>(e));
            if(last_dept && current_dep!= *last_dept){displayed=0;}
            if(!displayed){std::cout<<"Dept. "<<current_dep<<":\nName:\tId:\tSalary:\n";}
            last_dept=&std::get<3>(e);
            if(displayed < to_display){
                std::cout<<std::get<0>(e)<<'\t'<<std::get<1>(e)<<'\t'<<std::get<2>(e)<<std::endl;
            }
            ++displayed;
        });
	return 0;
}

```

