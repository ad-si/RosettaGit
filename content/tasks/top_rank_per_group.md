+++
title = "Top rank per group"
description = ""
date = 2019-10-18T20:35:13Z
aliases = []
[extra]
id = 3203
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "aime",
  "autohotkey",
  "awk",
  "bracmat",
  "c",
  "ceylon",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dyalect",
  "e",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "funl",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "nim",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pl_sql",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "scheme",
  "sidef",
  "smeql",
  "sql",
  "stata",
  "swift",
  "tcl",
  "tuscript",
  "txr",
  "ursala",
  "vba",
  "zkl",
]
+++

## Task

Find the top   ''N''   salaries in each department,   where   ''N''   is provided as a parameter.

Use this data as a formatted internal data structure (adapt it to your language-native idioms, rather than parse at runtime), or identify your external data source:

```txt

Employee Name,Employee ID,Salary,Department
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190

```





## Ada

top.adb:

```Ada
with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Top is
   type Departments is (D050, D101, D190, D202);
   type Employee_Data is record
      Name       : String (1 .. 15);
      ID         : String (1 .. 6);
      Salary     : Positive;
      Department : Departments;
   end record;

   package Employee_Vectors is new Ada.Containers.Vectors
     (Element_Type => Employee_Data, Index_Type => Positive);

   function Compare_Salary (Left, Right : Employee_Data) return Boolean is
   begin
      return Left.Salary > Right.Salary;
   end Compare_Salary;
   package Salary_Sort is new Employee_Vectors.Generic_Sorting
     ("<" => Compare_Salary);

   function Compare_Department (Left, Right : Employee_Data) return Boolean is
   begin
      return Left.Department < Right.Department;
   end Compare_Department;
   package Department_Sort is new Employee_Vectors.Generic_Sorting
     ("<" => Compare_Department);

   Example_Data : Employee_Vectors.Vector;
begin
   -- fill data
   Example_Data.Append (("Tyler Bennett  ", "E10297", 32000, D101));
   Example_Data.Append (("John Rappl     ", "E21437", 47000, D050));
   Example_Data.Append (("George Woltman ", "E00127", 53500, D101));
   Example_Data.Append (("Adam Smith     ", "E63535", 18000, D202));
   Example_Data.Append (("Claire Buckman ", "E39876", 27800, D202));
   Example_Data.Append (("David McClellan", "E04242", 41500, D101));
   Example_Data.Append (("Rich Holcomb   ", "E01234", 49500, D202));
   Example_Data.Append (("Nathan Adams   ", "E41298", 21900, D050));
   Example_Data.Append (("Richard Potter ", "E43128", 15900, D101));
   Example_Data.Append (("David Motsinger", "E27002", 19250, D202));
   Example_Data.Append (("Tim Sampair    ", "E03033", 27000, D101));
   Example_Data.Append (("Kim Arlich     ", "E10001", 57000, D190));
   Example_Data.Append (("Timothy Grove  ", "E16398", 29900, D190));
   -- sort by salary
   Salary_Sort.Sort (Example_Data);
   -- output each department
   for Department in Departments loop
      declare
         Position : Employee_Vectors.Cursor := Example_Data.First;
         Employee : Employee_Data;
      begin
         Ada.Text_IO.Put_Line ("Department " & Departments'Image (Department));
         for I in 1 .. 3 loop
            Employee := Employee_Vectors.Element (Position);
            while Employee.Department /= Department loop
               Position := Employee_Vectors.Next (Position);
               Employee := Employee_Vectors.Element (Position);
            end loop;
            Ada.Text_IO.Put_Line ("   " & Employee.Name & " | " &
                                  Employee.ID & " | " &
                                  Positive'Image (Employee.Salary));
            Position := Employee_Vectors.Next (Position);
         end loop;
      exception
         when Constraint_Error =>
            null;
      end;
   end loop;
end Top;
```
<pre style="height:30ex;overflow:scroll">Department D050
   John Rappl      | E21437 |  47000
   Nathan Adams    | E41298 |  21900
Department D101
   George Woltman  | E00127 |  53500
   David McClellan | E04242 |  41500
   Tyler Bennett   | E10297 |  32000
Department D190
   Kim Arlich      | E10001 |  57000
   Timothy Grove   | E16398 |  29900
Department D202
   Rich Holcomb    | E01234 |  49500
   Claire Buckman  | E39876 |  27800
   David Motsinger | E27002 |  19250
```



## Aime


```aime
Add_Employee(record employees, text name, id, integer salary, text department)
{
    employees[name] = list(name, id, salary, department);
}

collect(record top, employees)
{
    for (, list l in employees) {
        top.v_index(l[3]).v_list(l[2]).link(-1, l);
    }
    for (text department, index x in top) {
        list t;

        x.ucall(l_ucall, 0, l_append, 1, t);
        if (N < ~t.reverse) {
            t.erase(N, -1);
        }
        top[department] = t;
    }
}

print_department(text department, list employees)
{
    o_("Department ", department, "\n");

    for (, list l in employees) {
        o_form("  ~ | ~ | ~\n", l[0], l[1], l[2]);
    }
}

main(void)
{
    record employees, top;

    Add_Employee(employees, "Tyler Bennett  ", "E10297", 32000, "D101");
    Add_Employee(employees, "John Rappl     ", "E21437", 47000, "D050");
    Add_Employee(employees, "George Woltman ", "E00127", 53500, "D101");
    Add_Employee(employees, "Adam Smith     ", "E63535", 18000, "D202");
    Add_Employee(employees, "Claire Buckman ", "E39876", 27800, "D202");
    Add_Employee(employees, "David McClellan", "E04242", 41500, "D101");
    Add_Employee(employees, "Rich Holcomb   ", "E01234", 49500, "D202");
    Add_Employee(employees, "Nathan Adams   ", "E41298", 21900, "D050");
    Add_Employee(employees, "Richard Potter ", "E43128", 15900, "D101");
    Add_Employee(employees, "David Motsinger", "E27002", 19250, "D202");
    Add_Employee(employees, "Tim Sampair    ", "E03033", 27000, "D101");
    Add_Employee(employees, "Kim Arlich     ", "E10001", 57000, "D190");
    Add_Employee(employees, "Timothy Grove  ", "E16398", 29900, "D190");

    collect(top, employees);

    top.wcall(print_department, 0, 1);

    0;
}
```
Run as:

```txt
aime rcs/top_rank_per_group c N 5
```
<pre style="height:30ex;overflow:scroll">Department D050
  John Rappl      | E21437 | 47000
  Nathan Adams    | E41298 | 21900
Department D101
  George Woltman  | E00127 | 53500
  David McClellan | E04242 | 41500
  Tyler Bennett   | E10297 | 32000
  Tim Sampair     | E03033 | 27000
  Richard Potter  | E43128 | 15900
Department D190
  Kim Arlich      | E10001 | 57000
  Timothy Grove   | E16398 | 29900
Department D202
  Rich Holcomb    | E01234 | 49500
  Claire Buckman  | E39876 | 27800
  David Motsinger | E27002 | 19250
  Adam Smith      | E63535 | 18000
```



## AutoHotkey


```autohotkey
Departments = D050, D101, D190, D202
StringSplit, Department_, Departments, `,, %A_Space%

; Employee Name, Employee ID, Salary, Department
Add_Employee("Tyler Bennett  ", "E10297", 32000, "D101")
Add_Employee("John Rappl     ", "E21437", 47000, "D050")
Add_Employee("George Woltman ", "E00127", 53500, "D101")
Add_Employee("Adam Smith     ", "E63535", 18000, "D202")
Add_Employee("Claire Buckman ", "E39876", 27800, "D202")
Add_Employee("David McClellan", "E04242", 41500, "D101")
Add_Employee("Rich Holcomb   ", "E01234", 49500, "D202")
Add_Employee("Nathan Adams   ", "E41298", 21900, "D050")
Add_Employee("Richard Potter ", "E43128", 15900, "D101")
Add_Employee("David Motsinger", "E27002", 19250, "D202")
Add_Employee("Tim Sampair    ", "E03033", 27000, "D101")
Add_Employee("Kim Arlich     ", "E10001", 57000, "D190")
Add_Employee("Timothy Grove  ", "E16398", 29900, "D190")

; display top 3 ranks for each department
Loop, %Department_0% ; all departments
    MsgBox,, % "Department:  " Department_%A_Index%
           , % TopRank(3, Department_%A_Index%)

;---------------------------------------------------------------------------
TopRank(N, Department) { ; find the top N salaries in each department
;---------------------------------------------------------------------------
    local Collection := Msg := ""
    Loop, %m% ; all employees
        If (Employee_%A_Index%_Dept = Department)
            ; collect all the salaries being paid in this department
            Collection .= (Collection ? "," : "") Employee_%A_Index%_Salary
    Sort, Collection, ND,R
    StringSplit, Collection, Collection, `,
    Loop, % (N < Collection0) ? N : Collection0 {
        Salary := Collection%A_Index%
        Loop, %m% ; find the respective employee
            If (Employee_%A_Index%_Salary = Salary)
                ; and put out his/her details
                Msg .= Employee_%A_Index%_Name "`t"
                    .  Employee_%A_Index%_ID "`t"
                    .  Employee_%A_Index%_Salary "`t"
                    .  Employee_%A_Index%_Dept "`t`n"
    }
    Return, Msg
}

;---------------------------------------------------------------------------
Add_Employee(Name, ID, Salary, Department) {
;---------------------------------------------------------------------------
    global
    m++
    Employee_%m%_Name   := Name
    Employee_%m%_ID     := ID
    Employee_%m%_Salary := Salary
    Employee_%m%_Dept   := Department
}
```
<pre style="height:30ex;overflow:scroll">Department:  D050
---------------------------
John Rappl        E21437   47000   D050
Nathan Adams      E41298   21900   D050

Department:  D101
---------------------------
George Woltman    E00127   53500   D101
David McClellan   E04242   41500   D101
Tyler Bennett     E10297   32000   D101

Department:  D190
---------------------------
Kim Arlich        E10001   57000   D190
Timothy Grove     E16398   29900   D190

Department:  D202
---------------------------
Rich Holcomb      E01234   49500   D202
Claire Buckman    E39876   27800   D202
David Motsinger   E27002   19250   D202
```


## AWK


```AWK

# syntax: GAWK -f TOP_RANK_PER_GROUP.AWK [n]
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
BEGIN {
    arrA[++n] = "Employee Name,Employee ID,Salary,Department" # raw data
    arrA[++n] = "Tyler Bennett,E10297,32000,D101"
    arrA[++n] = "John Rappl,E21437,47000,D050"
    arrA[++n] = "George Woltman,E00127,53500,D101"
    arrA[++n] = "Adam Smith,E63535,18000,D202"
    arrA[++n] = "Claire Buckman,E39876,27800,D202"
    arrA[++n] = "David McClellan,E04242,41500,D101"
    arrA[++n] = "Rich Holcomb,E01234,49500,D202"
    arrA[++n] = "Nathan Adams,E41298,21900,D050"
    arrA[++n] = "Richard Potter,E43128,15900,D101"
    arrA[++n] = "David Motsinger,E27002,19250,D202"
    arrA[++n] = "Tim Sampair,E03033,27000,D101"
    arrA[++n] = "Kim Arlich,E10001,57000,D190"
    arrA[++n] = "Timothy Grove,E16398,29900,D190"
    for (i=2; i<=n; i++) { # build internal structure
      split(arrA[i],arrB,",")
      arrC[arrB[4]][arrB[3]][arrB[2] " " arrB[1]] # I.E. arrC[dept][salary][id " " name]
    }
    show = (ARGV[1] == "") ? 1 : ARGV[1] # employees to show per department
    printf("DEPT SALARY EMPID  NAME\n\n") # produce report
    PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 1
    for (i in arrC) {
      PROCINFO["sorted_in"] = "@ind_str_desc" ; SORTTYPE = 9
      shown = 0
      for (j in arrC[i]) {
        PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 1
        for (k in arrC[i][j]) {
          if (shown++ < show) {
            printf("%-4s %6s %s\n",i,j,k)
            printed++
          }
        }
      }
      if (printed > 0) { print("") }
      printed = 0
    }
    exit(0)
}

```

<p>Sample command and output:</p>

```txt

GAWK -f TOP_RANK_PER_GROUP.AWK 3

DEPT SALARY EMPID  NAME

D050  47000 E21437 John Rappl
D050  21900 E41298 Nathan Adams

D101  53500 E00127 George Woltman
D101  41500 E04242 David McClellan
D101  32000 E10297 Tyler Bennett

D190  57000 E10001 Kim Arlich
D190  29900 E16398 Timothy Grove

D202  49500 E01234 Rich Holcomb
D202  27800 E39876 Claire Buckman
D202  19250 E27002 David Motsinger

```



## Bracmat

Bracmat has no dedicated sorting functions. However, when evaluating algebraic expressions, Bracmat sorts factors in products and terms in sums. Moreover, Bracmat simplifies products by, amongst other transformations, collecting exponents of the same base into a single exponent, which is the sum of the collected exponents: <code>a^b*a^c</code> &rarr; <code>a^(b+c)</code>. This built-in behaviour is made use of in this solution.

```bracmat
      (Tyler Bennett,E10297,32000,D101)
      (John Rappl,E21437,47000,D050)
      (George Woltman,E00127,53500,D101)
      (Adam Smith,E63535,18000,D202)
      (Claire Buckman,E39876,27800,D202)
      (David McClellan,E04242,41500,D101)
      (Rich Holcomb,E01234,49500,D202)
      (Nathan Adams,E41298,21900,D050)
      (Richard Potter,E43128,15900,D101)
      (David Motsinger,E27002,19250,D202)
      (Tim Sampair,E03033,27000,D101)
      (Kim Arlich,E10001,57000,D190)
      (Timothy Grove,E16398,29900,D190)
  : ?employees
& ( toprank
  =   employees N n P "Employee Name" "Employee ID" SalaryDepartment
    .   !arg:(?employees.?N)
      & 1:?P
      &   whl
        ' (   !employees
            :   (?"Employee Name",?"Employee ID",?Salary,?Department)
                ?employees
          & !Department^(!Salary.!"Employee Name".!"Employee ID")*!P:?P
          )
      & out$(Top !N "salaries per department.")
      &   whl
        ' ( !P:%?Department^?employees*?P
          & out$(str$("Department " !Department ":"))
          & !N:?n
          &   whl
            ' ( !n+-1:~<0:?n
              &   !employees
                : ?employees+(?Salary.?"Employee Name".?"Employee ID")
              &   out
                $ (str$("  " !"Employee Name" " (" !"Employee ID" "):" !Salary))
              )
          )
  )
& toprank$(!employees.3)
& ;
```
<pre style="height:30ex;overflow:scroll">Top 3 salaries per department.
Department D050:
  JohnRappl (E21437):47000
  NathanAdams (E41298):21900
Department D101:
  GeorgeWoltman (E00127):53500
  DavidMcClellan (E04242):41500
  TylerBennett (E10297):32000
Department D190:
  KimArlich (E10001):57000
  TimothyGrove (E16398):29900
Department D202:
  RichHolcomb (E01234):49500
  ClaireBuckman (E39876):27800
  DavidMotsinger (E27002):19250
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
	const char *name, *id, *dept;
	int sal;
} person;

person ppl[] = {
	{"Tyler Bennett",	"E10297", "D101", 32000},
	{"John Rappl",		"E21437", "D050", 47000},
	{"George Woltman",	"E00127", "D101", 53500},
	{"Adam Smith",		"E63535", "D202", 18000},
	{"Claire Buckman",	"E39876", "D202", 27800},
	{"David McClellan",	"E04242", "D101", 41500},
	{"Rich Holcomb",	"E01234", "D202", 49500},
	{"Nathan Adams",	"E41298", "D050", 21900},
	{"Richard Potter",	"E43128", "D101", 15900},
	{"David Motsinger",	"E27002", "D202", 19250},
	{"Tim Sampair",		"E03033", "D101", 27000},
	{"Kim Arlich",		"E10001", "D190", 57000},
	{"Timothy Grove",	"E16398", "D190", 29900},
};

int pcmp(const void *a, const void *b)
{
	const person *aa = a, *bb = b;
	int x = strcmp(aa->dept, bb->dept);
	if (x) return x;
	return aa->sal > bb->sal ? -1 : aa->sal < bb->sal;
}

#define N sizeof(ppl)/sizeof(person)
void top(int n)
{
	int i, rank;
	qsort(ppl, N, sizeof(person), pcmp);

	for (i = rank = 0; i < N; i++) {
		if (i && strcmp(ppl[i].dept, ppl[i - 1].dept)) {
			rank = 0;
			printf("\n");
		}

		if (rank++ < n)
			printf("%s %d: %s\n", ppl[i].dept, ppl[i].sal, ppl[i].name);
	}
}

int main()
{
	top(2);
	return 0;
}
```
```txt
D050 47000: John Rappl
D050 21900: Nathan Adams

D101 53500: George Woltman
D101 41500: David McClellan

D190 57000: Kim Arlich
D190 29900: Timothy Grove

D202 49500: Rich Holcomb
D202 27800: Claire Buckman
```



## C++


```cpp
#include <string>
#include <set>
#include <list>
#include <map>
#include <iostream>


struct Employee
{
	std::string Name;
	std::string ID;
	unsigned long Salary;
	std::string Department;
	Employee(std::string _Name = "", std::string _ID = "", unsigned long _Salary = 0, std::string _Department = "")
	: Name(_Name), ID(_ID), Salary(_Salary), Department(_Department)
	{ }

	void display(std::ostream& out) const
	{
		out << Name << "\t" << ID << "\t" << Salary << "\t" << Department << std::endl;
	}
};

// We'll tell std::set to use this to sort our employees.
struct CompareEarners
{
	bool operator()(const Employee& e1, const Employee& e2)
	{
		return (e1.Salary > e2.Salary);
	}
};

// A few typedefs to make the code easier to type, read and maintain.
typedef std::list<Employee> EMPLOYEELIST;

// Notice the CompareEarners; We're telling std::set to user our specified comparison mechanism
// to sort its contents.
typedef std::set<Employee, CompareEarners> DEPARTMENTPAYROLL;

typedef std::map<std::string, DEPARTMENTPAYROLL> DEPARTMENTLIST;

void initialize(EMPLOYEELIST& Employees)
{
	// Initialize our employee list data source.
	Employees.push_back(Employee("Tyler Bennett", "E10297", 32000, "D101"));
	Employees.push_back(Employee("John Rappl", "E21437", 47000, "D050"));
	Employees.push_back(Employee("George Woltman", "E21437", 53500, "D101"));
	Employees.push_back(Employee("Adam Smith", "E21437", 18000, "D202"));
	Employees.push_back(Employee("Claire Buckman", "E39876", 27800, "D202"));
	Employees.push_back(Employee("David McClellan", "E04242", 41500, "D101"));
	Employees.push_back(Employee("Rich Holcomb", "E01234", 49500, "D202"));
	Employees.push_back(Employee("Nathan Adams", "E41298", 21900, "D050"));
	Employees.push_back(Employee("Richard Potter", "E43128", 15900, "D101"));
	Employees.push_back(Employee("David Motsinger", "E27002", 19250, "D202"));
	Employees.push_back(Employee("Tim Sampair", "E03033", 27000, "D101"));
	Employees.push_back(Employee("Kim Arlich", "E10001", 57000, "D190"));
	Employees.push_back(Employee("Timothy Grove", "E16398", 29900, "D190"));
}

void group(EMPLOYEELIST& Employees, DEPARTMENTLIST& Departments)
{
	// Loop through all of our employees.
	for( EMPLOYEELIST::iterator iEmployee = Employees.begin();
		 Employees.end() != iEmployee;
		 ++iEmployee )
	{
		DEPARTMENTPAYROLL& groupSet = Departments[iEmployee->Department];

		// Add our employee to this group.
		groupSet.insert(*iEmployee);
	}
}

void present(DEPARTMENTLIST& Departments, unsigned int N)
{
	// Loop through all of our departments
	for( DEPARTMENTLIST::iterator iDepartment = Departments.begin();
		 Departments.end() != iDepartment;
		 ++iDepartment )
	{
		std::cout << "In department " << iDepartment->first << std::endl;
		std::cout << "Name\t\tID\tSalary\tDepartment" << std::endl;
		// Get the top three employees for each employee
		unsigned int rank = 1;
		for( DEPARTMENTPAYROLL::iterator iEmployee = iDepartment->second.begin();
			 ( iDepartment->second.end() != iEmployee) && (rank <= N);
			 ++iEmployee, ++rank )
		{
			iEmployee->display(std::cout);
		}
		std::cout << std::endl;
	}
}

int main(int argc, char* argv[])
{
	// Our container for our list of employees.
	EMPLOYEELIST Employees;

	// Fill our list of employees
	initialize(Employees);

	// Our departments.
	DEPARTMENTLIST Departments;

	// Sort our employees into their departments.
	// This will also rank them.
	group(Employees, Departments);

	// Display the top 3 earners in each department.
	present(Departments, 3);

	return 0;
}
```
<pre style="height:30ex;overflow:scroll">In department D050
Name            ID      Salary  Department
John Rappl      E21437  47000   D050
Nathan Adams    E41298  21900   D050

In department D101
Name            ID      Salary  Department
George Woltman  E21437  53500   D101
David McClellan E04242  41500   D101
Tyler Bennett   E10297  32000   D101

In department D190
Name            ID      Salary  Department
Kim Arlich      E10001  57000   D190
Timothy Grove   E16398  29900   D190

In department D202
Name            ID      Salary  Department
Rich Holcomb    E01234  49500   D202
Claire Buckman  E39876  27800   D202
David Motsinger E27002  19250   D202
```


## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
	class Employee
	{
		public Employee(string name, string id, int salary, string department)
		{
			Name = name;
			Id = id;
			Salary = salary;
			Department = department;
		}

		public string Name { get; private set; }
		public string Id { get; private set; }
		public int Salary { get; private set; }
		public string Department { get; private set; }

		public override string ToString()
		{
			return String.Format("{0, -25}\t{1}\t{2}", Name, Id, Salary);
		}
	}

	private static void Main(string[] args)
	{
		var employees = new List<Employee>
			            {
				            new Employee("Tyler Bennett", "E10297", 32000, "D101"),
				            new Employee("John Rappl", "E21437", 47000, "D050"),
				            new Employee("George Woltman", "E21437", 53500, "D101"),
				            new Employee("Adam Smith", "E21437", 18000, "D202"),
				            new Employee("Claire Buckman", "E39876", 27800, "D202"),
				            new Employee("David McClellan", "E04242", 41500, "D101"),
				            new Employee("Rich Holcomb", "E01234", 49500, "D202"),
				            new Employee("Nathan Adams", "E41298", 21900, "D050"),
				            new Employee("Richard Potter", "E43128", 15900, "D101"),
				            new Employee("David Motsinger", "E27002", 19250, "D202"),
				            new Employee("Tim Sampair", "E03033", 27000, "D101"),
				            new Employee("Kim Arlich", "E10001", 57000, "D190"),
				            new Employee("Timothy Grove", "E16398", 29900, "D190")
			            };

		DisplayTopNPerDepartment(employees, 2);
	}

	static void DisplayTopNPerDepartment(IEnumerable<Employee> employees, int n)
	{
		var topSalariesByDepartment =
			from employee in employees
			group employee by employee.Department
			into g
			select new
				    {
					    Department = g.Key,
					    TopEmployeesBySalary = g.OrderByDescending(e => e.Salary).Take(n)
				    };

		foreach (var x in topSalariesByDepartment)
		{
			Console.WriteLine("Department: " + x.Department);
			foreach (var employee in x.TopEmployeesBySalary)
				Console.WriteLine(employee);
			Console.WriteLine("----------------------------");
		}
	}
}
```
<pre style="height:30ex;overflow:scroll">
Department: D101
George Woltman           	E21437	53500
David McClellan          	E04242	41500
----------------------------
Department: D050
John Rappl               	E21437	47000
Nathan Adams             	E41298	21900
----------------------------
Department: D202
Rich Holcomb             	E01234	49500
Claire Buckman           	E39876	27800
----------------------------
Department: D190
Kim Arlich               	E10001	57000
Timothy Grove            	E16398	29900
----------------------------
```


Online demo: http://ideone.com/95TxAV


## Ceylon


```ceylon
class Employee(name, id, salary, dept) {
    shared String name;
    shared String id;
    shared Integer salary;
    shared String dept;

    string => "``name`` ``id`` $``salary``.00 ``dept``";
}

Employee[] employees = [
    Employee("Tyler Bennett", "E10297", 32000, "D101"),
    Employee("John Rappl", "E21437", 47000, "D050"),
    Employee("George Woltman", "E00127", 53500, "D101"),
    Employee("Adam Smith", "E63535", 18000, "D202"),
    Employee("Claire Buckman", "E39876", 27800, "D202"),
    Employee("David McClellan", "E04242", 41500, "D101"),
    Employee("Rich Holcomb", "E01234", 49500, "D202"),
    Employee("Nathan Adams", "E41298", 21900, "D050"),
    Employee("Richard Potter", "E43128", 15900, "D101"),
    Employee("David Motsinger", "E27002", 19250, "D202"),
    Employee("Tim Sampair", "E03033", 27000, "D101"),
    Employee("Kim Arlich", "E10001", 57000, "D190"),
    Employee("Timothy Grove", "E16398", 29900, "D190")
];

"This is the main function."
shared void run() {

    value topRanked = topSalaries(employees, 3);

    for (dept -> staff in topRanked) {
        print(dept);
        for (employee in staff) {
            print("\t``employee``");
        }
    }
}

Map<String, {Employee*}> topSalaries({Employee*} employees, Integer n) => map {
    for (dept -> staff in employees.group(Employee.dept))
    dept -> staff.sort(byDecreasing(Employee.salary)).take(n)
};
```



## Clojure



```lisp
(use '[clojure.contrib.seq-utils :only (group-by)])

(defstruct employee :Name :ID :Salary :Department)

(def data
     (->> '(("Tyler Bennett" E10297 32000 D101)
            ("John Rappl" E21437 47000 D050)
            ("George Woltman" E00127 53500 D101)
            ("Adam Smith" E63535 18000 D202)
            ("Claire Buckman" E39876 27800 D202)
            ("David McClellan" E04242 41500 D101)
            ("Rich Holcomb" E01234 49500 D202)
            ("Nathan Adams" E41298 21900 D050)
            ("Richard Potter" E43128 15900 D101)
            ("David Motsinger" E27002 19250 D202)
            ("Tim Sampair" E03033 27000 D101)
            ("Kim Arlich" E10001 57000 D190)
            ("Timothy Grove" E16398 29900 D190))
          (map #(apply (partial struct employee) %))))


(doseq [[dep emps] (group-by :Department data)]
  (println "Department:" dep)
  (doseq [e (take 3 (reverse (sort-by :Salary emps)))]
    (println e)))

```
<pre style="height:30ex;overflow:scroll">Department: D050
{:Name John Rappl, :ID E21437, :Salary 47000, :Department D050}
{:Name Nathan Adams, :ID E41298, :Salary 21900, :Department D050}
Department: D101
{:Name George Woltman, :ID E00127, :Salary 53500, :Department D101}
{:Name David McClellan, :ID E04242, :Salary 41500, :Department D101}
{:Name Tyler Bennett, :ID E10297, :Salary 32000, :Department D101}
Department: D190
{:Name Kim Arlich, :ID E10001, :Salary 57000, :Department D190}
{:Name Timothy Grove, :ID E16398, :Salary 29900, :Department D190}
Department: D202
{:Name Rich Holcomb, :ID E01234, :Salary 49500, :Department D202}
{:Name Claire Buckman, :ID E39876, :Salary 27800, :Department D202}
{:Name David Motsinger, :ID E27002, :Salary 19250, :Department D202}

```



## Common Lisp


```lisp
(defun top-n-by-group (n data value-key group-key predicate &key (group-test 'eql))
  (let ((not-pred (complement predicate))
        (group-data (make-hash-table :test group-test)))
    (labels ((value (datum)
               (funcall value-key datum))
             (insert (x list)
               (merge 'list (list x) list not-pred :key #'value))
             (entry (group)
               "Return the entry for the group, creating it if
                necessary. An entry is a list whose first element is
                k, the number of items currently associated with the
                group (out of n total), and whose second element is
                the list of the k current top items for the group."
               (multiple-value-bind (entry presentp)
                   (gethash group group-data)
                 (if presentp entry
                   (setf (gethash group group-data)
                         (list 0 '())))))
             (update-entry (entry datum)
               "Update the entry using datum. If there are already n
                items associated with the entry, then when datum's value
                is greater than the current least item, data is merged into
                the items, and the list (minus the first element) is
                stored in entry. Otherwise, if there are fewer than n
                items in the entry, datum is merged in, and the
                entry's k is increased by 1."
               (if (= n (first entry))
                 (when (funcall predicate (value datum) (value (first (second entry))))
                   (setf (second entry)
                         (cdr (insert datum (second entry)))))
                 (setf (first entry) (1+ (first entry))
                       (second entry) (insert datum (second entry))))))
      (dolist (datum data group-data)
        (update-entry (entry (funcall group-key datum)) datum)))))
```

Example

```lisp
>
 (defparameter *employee-data*
  '(("Tyler Bennett" E10297 32000 D101)
    ("John Rappl" E21437 47000 D050)
    ("George Woltman" E00127 53500 D101)
    ("Adam Smith" E63535 18000 D202)
    ("Claire Buckman" E39876 27800 D202)
    ("David McClellan" E04242 41500 D101)
    ("Rich Holcomb" E01234 49500 D202)
    ("Nathan Adams" E41298 21900 D050)
    ("Richard Potter" E43128 15900 D101)
    ("David Motsinger" E27002 19250 D202)
    ("Tim Sampair" E03033 27000 D101)
    ("Kim Arlich" E10001 57000 D190)
    ("Timothy Grove" E16398 29900 D190))
  "A list of lists of each employee's name, id, salary, and
department.")
*EMPLOYEE-DATA*

> (top-n-by-group 3 *employee-data* 'third 'fourth '>)
#<EQL Hash Table{4} 2361A0E7>

> (describe *)

#<EQL Hash Table{4} 2361A0E7> is a HASH-TABLE
D101      (3 (("Tyler Bennett" E10297 32000 D101) ("David McClellan" E04242 41500 D101) ("George Woltman" E00127 53500 D101)))
D050      (2 (("Nathan Adams" E41298 21900 D050) ("John Rappl" E21437 47000 D050)))
D202      (3 (("David Motsinger" E27002 19250 D202) ("Claire Buckman" E39876 27800 D202) ("Rich Holcomb" E01234 49500 D202)))
D190      (2 (("Timothy Grove" E16398 29900 D190) ("Kim Arlich" E10001 57000 D190)))
```



## D


```d
import std.stdio, std.algorithm, std.conv, std.range;

struct Employee {
  string name, id;
  uint salary;
  string department;
}

immutable Employee[] data = [
    {"Tyler Bennett",   "E10297", 32_000, "D101"},
    {"John Rappl",      "E21437", 47_000, "D050"},
    {"George Woltman",  "E00127", 53_500, "D101"},
    {"Adam Smith",      "E63535", 18_000, "D202"},
    {"Claire Buckman",  "E39876", 27_800, "D202"},
    {"David McClellan", "E04242", 41_500, "D101"},
    {"Rich Holcomb",    "E01234", 49_500, "D202"},
    {"Nathan Adams",    "E41298", 21_900, "D050"},
    {"Richard Potter",  "E43128", 15_900, "D101"},
    {"David Motsinger", "E27002", 19_250, "D202"},
    {"Tim Sampair",     "E03033", 27_000, "D101"},
    {"Kim Arlich",      "E10001", 57_000, "D190"},
    {"Timothy Grove",   "E16398", 29_900, "D190"}];

void main(in string[] args) {
  immutable n = (args.length == 2) ? to!int(args[1]) : 3;

  Employee[][string] departments;
  foreach (immutable rec; data)
    departments[rec.department] ~= rec;

  foreach (dep, recs; departments) {
    recs.topN!q{a.salary > b.salary}(n);
    writefln("Department %s\n  %(%s\n  %)\n", dep, recs.take(n));
  }
}
```
<pre style="height:30ex;overflow:scroll">Department D202
  Employee("Rich Holcomb", "E01234", 49500, "D202")
  Employee("Claire Buckman", "E39876", 27800, "D202")
  Employee("David Motsinger", "E27002", 19250, "D202")

Department D190
  Employee("Kim Arlich", "E10001", 57000, "D190")
  Employee("Timothy Grove", "E16398", 29900, "D190")

Department D101
  Employee("George Woltman", "E00127", 53500, "D101")
  Employee("David McClellan", "E04242", 41500, "D101")
  Employee("Tyler Bennett", "E10297", 32000, "D101")

Department D050
  Employee("John Rappl", "E21437", 47000, "D050")
  Employee("Nathan Adams", "E41298", 21900, "D050")
```



## Dyalect



```dyalect
type Employee(name,id,salary,department)

func Employee.toString() {
    "$\(this.salary) (name: \(this.name), id: \(this.id), department: \(this.department)"
}

const employees = [
    Employee("Tyler Bennett","E10297",32000,"D101"),
    Employee("John Rappl","E21437",47000,"D050"),
    Employee("George Woltman","E00127",53500,"D101"),
    Employee("Adam Smith","E63535",18000,"D202"),
    Employee("Claire Buckman","E39876",27800,"D202"),
    Employee("David McClellan","E04242",41500,"D101"),
    Employee("Rich Holcomb","E01234",49500,"D202"),
    Employee("Nathan Adams","E41298",21900,"D050"),
    Employee("Richard Potter","E43128",15900,"D101"),
    Employee("David Motsinger","E27002",19250,"D202"),
    Employee("Tim Sampair","E03033",27000,"D101"),
    Employee("Kim Arlich","E10001",57000,"D190"),
    Employee("Timothy Grove","E16398",29900,"D190")
]

func topNSalaries(n) {
    //We sort employees based on salary
    employees.sort($1.salary - $0.salary)
    const max = if n > employees.len() - 1 { employees.len() - 1 } else { n }
    for i in 0..max {
        yield employees[i]
    }
}

for e in topNSalaries(5) {
    print(e)
}
```


```txt
$57000 (name: Kim Arlich, id: E10001, department: D190
$53500 (name: George Woltman, id: E00127, department: D101
$49500 (name: Rich Holcomb, id: E01234, department: D202
$47000 (name: John Rappl, id: E21437, department: D050
$41500 (name: David McClellan, id: E04242, department: D101
$32000 (name: Tyler Bennett, id: E10297, department: D101
```



## E


```e
/** Turn a list of arrays into a list of maps with the given keys. */
def addKeys(keys, rows) {
  def res := [].diverge()
  for row in rows { res.push(__makeMap.fromColumns(keys, row)) }
  return res.snapshot()
}

def data := addKeys(
  ["name",            "id",  "salary", "dept"],
 [["Tyler Bennett",   "E10297", 32000, "D101"],
  ["John Rappl",      "E21437", 47000, "D050"],
  ["George Woltman",  "E00127", 53500, "D101"],
  ["Adam Smith",      "E63535", 18000, "D202"],
  ["Claire Buckman",  "E39876", 27800, "D202"],
  ["David McClellan", "E04242", 41500, "D101"],
  ["Rich Holcomb",    "E01234", 49500, "D202"],
  ["Nathan Adams",    "E41298", 21900, "D050"],
  ["Richard Potter",  "E43128", 15900, "D101"],
  ["David Motsinger", "E27002", 19250, "D202"],
  ["Tim Sampair",     "E03033", 27000, "D101"],
  ["Kim Arlich",      "E10001", 57000, "D190"],
  ["Timothy Grove",   "E16398", 29900, "D190"]])

def topSalaries(n, out) {
    var groups := [].asMap()
    for row in data {
        def [=> salary, => dept] | _ := row
        def top := groups.fetch(dept, fn {[]}).with([-salary, row]).sort()
        groups with= (dept, top.run(0, top.size().min(n)))
    }
    for dept => group in groups.sortKeys() {
        out.println(`Department $dept`)
        out.println(`---------------`)
        for [_, row] in group {
          out.println(`${row["id"]}  $$${row["salary"]}  ${row["name"]}`)
        }
        out.println()
    }
}
```


Note: This uses an append-and-then-sort to maintain the list of top N; a sorted insert or a proper [[wp: selection algorithm|selection algorithm]] would be more efficient. As long as <var>N</var> is small, this does not matter much; the algorithm is O(n) with respect to the data set.{{out}}
<pre style="height:30ex;overflow:scroll">? topSalaries(3, stdout)
Department D050
---------------
E21437  $47000  John Rappl
E41298  $21900  Nathan Adams

Department D101
---------------
E00127  $53500  George Woltman
E04242  $41500  David McClellan
E10297  $32000  Tyler Bennett

Department D190
---------------
E10001  $57000  Kim Arlich
E16398  $29900  Timothy Grove

Department D202
---------------
E01234  $49500  Rich Holcomb
E39876  $27800  Claire Buckman
E27002  $19250  David Motsinger
```



## EchoLisp

We use the '''sql.lib''' to select, sort and group records from the table 'emps'. The output is appended in a new table, 'high'.

```scheme

(lib 'struct) ;; tables are based upon structures
(lib 'sql)  ;; sql-select function

;; input table
(define emps  (make-table (struct emp (name id salary dept))))
;; output table
(define high  (make-table (struct out (dept name salary))))

;; sort/group procedure
(define (get-high num-records: N into: high)
(sql-select emp.dept emp.name emp.salary
  from emps
  group-by emp.dept
  order-by emp.salary desc limit N into high))

```

```scheme

(define emps_file
'(("Tyler Bennett" E10297 32000 D101)
("John Rappl" E21437 47000 D050)
("George Woltman" E00127 53500 D101)
("Adam Smith" E63535 18000 D202)
("Claire Buckman" E39876 27800 D202)
("David McClellan" E04242 41500 D101)
("Rich Holcomb" E01234 49500 D202)
("Simon Gallubert" E00000 42 D666)
("Nathan Adams" E41298 21900 D050)
("Richard Potter" E43128 15900 D101)
("David Motsinger" E27002 19250 D202)
("Tim Sampair" E03033 27000 D101)
("Kim Arlich" E10001 57000 D190)
("Timothy Grove" E16398 29900 D190)))

(list->table emps_file emps ) ;; load the table

(get-high 2 high)
(table-print high)

[0]   D050  John Rappl       47000
[1]   D050  Nathan Adams     21900
[2]   D101  George Woltman   53500
[3]   D101  David McClellan  41500
[4]   D190  Kim Arlich       57000
[5]   D190  Timothy Grove    29900
[6]   D202  Rich Holcomb     49500
[7]   D202  Claire Buckman   27800
[8]   D666  Simon Gallubert  42

(sql-delete from high)
(get-high 1 high)
(table-print high)
[0]   D050  John Rappl       47000
[1]   D101  George Woltman   53500
[2]   D190  Kim Arlich       57000
[3]   D202  Rich Holcomb     49500
[4]   D666  Simon Gallubert  42

```



```



```


## Elena

ELENA 4.1 :

```elena
import system'collections;
import system'routines;
import extensions;
import extensions'routines;
import extensions'text;

class Employee
{
    prop string Name;
    prop string ID;
    prop int    Salary;
    prop string Department;

    string Printable
        = new StringWriter()
            .writePaddingRight(Name, 25)
            .writePaddingRight(ID, 12)
            .writePaddingRight(Salary.Printable, 12)
            .write:Department;
}

extension reportOp
{
    topNPerDepartment(n)
        = self.groupBy:(x => x.Department ).selectBy:(x)
        {
            ^ new :: {
                Department = x.Key;

                Employees
                    = x.orderBy:(f,l => f.Salary > l.Salary ).top(n).summarize(new ArrayList());
            }
        };
}

public program()
{
    var employees := new Employee[]::
    (
        new Employee::{ this Name := "Tyler Bennett"; this ID := "E10297"; this Salary:=32000; this Department:="D101";},
        new Employee::{ this Name := "John Rappl"; this ID := "E21437"; this Salary:=47000; this Department:="D050";},
        new Employee::{ this Name := "George Woltman"; this ID := "E00127"; this Salary:=53500; this Department:="D101";},
        new Employee::{ this Name := "Adam Smith"; this ID := "E63535"; this Salary:=18000; this Department:="D202";},
        new Employee::{ this Name := "Claire Buckman"; this ID := "E39876"; this Salary:=27800; this Department:="D202";},
        new Employee::{ this Name := "David McClellan"; this ID := "E04242"; this Salary:=41500; this Department:="D101";},
        new Employee::{ this Name := "Rich Holcomb"; this ID := "E01234"; this Salary:=49500; this Department:="D202";},
        new Employee::{ this Name := "Nathan Adams"; this ID := "E41298"; this Salary:=21900; this Department:="D050";},
        new Employee::{ this Name := "Richard Potter"; this ID := "E43128"; this Salary:=15900; this Department:="D101";},
        new Employee::{ this Name := "David Motsinger"; this ID := "E27002"; this Salary:=19250; this Department:="D202";},
        new Employee::{ this Name := "Tim Sampair"; this ID := "E03033"; this Salary:=27000; this Department:="D101";},
        new Employee::{ this Name := "Kim Arlich"; this ID := "E10001"; this Salary:=57000; this Department:="D190";},
        new Employee::{ this Name := "Timothy Grove"; this ID := "E16398"; this Salary:=29900; this Department:="D190";}
    );

    employees.topNPerDepartment:2.forEach:(info)
    {
        console.printLine("Department: ",info.Department);

        info.Employees.forEach:printingLn;

        console.writeLine:"---------------------------------------------"
    };

    console.readChar()
}
```

```txt

Department: D101
George Woltman           E00127      53500       D101
David McClellan          E04242      41500       D101
---------------------------------------------
Department: D050
John Rappl               E21437      47000       D050
Nathan Adams             E41298      21900       D050
---------------------------------------------
Department: D202
Rich Holcomb             E01234      49500       D202
Claire Buckman           E39876      27800       D202
---------------------------------------------
Department: D190
Kim Arlich               E10001      57000       D190
Timothy Grove            E16398      29900       D190
---------------------------------------------

```



## Elixir

Quick implementation using piping and anonymous functions.

```Elixir
defmodule TopRank do
  def per_groupe(data, n) do
    String.split(data, ~r/(\n|\r\n|\r)/, trim: true)
    |> Enum.drop(1)
    |> Enum.map(fn person -> String.split(person,",") end)
    |> Enum.group_by(fn person -> department(person) end)
    |> Enum.each(fn {department,group} ->
         IO.puts "Department: #{department}"
         Enum.sort_by(group, fn person -> -salary(person) end)
         |> Enum.take(n)
         |> Enum.each(fn person -> IO.puts str_format(person) end)
       end)
  end

  defp salary([_,_,x,_]), do: String.to_integer(x)
  defp department([_,_,_,x]), do: x
  defp str_format([a,b,c,_]), do: "  #{a} - #{b} - #{c} annual salary"
end

data = """
Employee Name,Employee ID,Salary,Department
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190
"""
TopRank.per_groupe(data, 3)
```


```txt

Department: D050
  John Rappl - E21437 - 47000 annual salary
  Nathan Adams - E41298 - 21900 annual salary
Department: D101
  George Woltman - E00127 - 53500 annual salary
  David McClellan - E04242 - 41500 annual salary
  Tyler Bennett - E10297 - 32000 annual salary
Department: D190
  Kim Arlich - E10001 - 57000 annual salary
  Timothy Grove - E16398 - 29900 annual salary
Department: D202
  Rich Holcomb - E01234 - 49500 annual salary
  Claire Buckman - E39876 - 27800 annual salary
  David Motsinger - E27002 - 19250 annual salary

```



## Erlang


```Erlang
<-module( top_rank_per_group  ).

-export( [employees/0, employees_in_department/2, highest_payed/2, task/1] ).

-record( employee, {name, id, salery, department} ).

employees() ->
	[#employee{name="Tyler Bennett", id="E10297", salery=32000, department="D101"},
		#employee{name="John Rappl", id="E21437", salery=47000, department="D101"},
		#employee{name="George Woltman", id="E00127", salery=53500, department="D050"},
		#employee{name="Adam Smith", id="E63535", salery=18000, department="D202"},
		#employee{name="Claire Buckman", id="E39876", salery=27800, department="D202"},
		#employee{name="David McClellan", id="E04242", salery=41500, department="D101"},
		#employee{name="Rich Holcomb", id="E01234", salery=49500, department="D202"},
		#employee{name="Nathan Adams", id="E41298", salery=21900, department="D050"},
		#employee{name="Richard Potter", id="E43128", salery=15900, department="D101"},
		#employee{name="David Motsinger", id="E27002", salery=19250, department="D202"},
		#employee{name="Tim Sampair", id="E03033", salery=27000, department="D101"},
		#employee{name="Kim Arlich", id="E10001", salery=57000, department="D190"},
		#employee{name="Timothy Grove", id="E16398", salery=29900, department="D190"}].

employees_in_department( Department, Employees ) -> [X || #employee{department=D}=X <- Employees, D =:= Department].

highest_payed( N, Employees ) ->
	{Highest, _T} = lists:split( N, lists:reverse(lists:keysort(#employee.salery, Employees)) ),
	Highest.

task( N ) ->
	Employees = employees(),
	Departments = lists:usort( [X || #employee{department=X} <- Employees] ),
	Employees_in_departments = [employees_in_department(X, Employees) || X <- Departments],
	Highest_payed_in_departments = [highest_payed(N, Xs) || Xs <- Employees_in_departments],
	[task_write(X) || X <- Highest_payed_in_departments].



task_write( Highest_payeds ) ->
	[io:fwrite( "~p ~p: ~p~n", [Department, Salery, Name]) || #employee{department=Department, salery=Salery, name=Name} <- Highest_payeds],
	io:nl().

```
```txt
<14> top_rank_per_group:task(2).
"D050" 53500: "George Woltman"
"D050" 21900: "Nathan Adams"

"D101" 47000: "John Rappl"
"D101" 41500: "David McClellan"

"D190" 57000: "Kim Arlich"
"D190" 29900: "Timothy Grove"

"D202" 49500: "Rich Holcomb"
"D202" 27800: "Claire Buckman"
```


=={{header|F Sharp|F#}}==

```fsharp
let data =
  [
    "Tyler Bennett",   "E10297",  32000,  "D101";
    "John Rappl",      "E21437",  47000,  "D050";
    "George Woltman",  "E00127",  53500,  "D101";
    "Adam Smith",      "E63535",  18000,  "D202";
    "Claire Buckman",  "E39876",  27800,  "D202";
    "David McClellan", "E04242",  41500,  "D101";
    "Rich Holcomb",    "E01234",  49500,  "D202";
    "Nathan Adams",    "E41298",  21900,  "D050";
    "Richard Potter",  "E43128",  15900,  "D101";
    "David Motsinger", "E27002",  19250,  "D202";
    "Tim Sampair",     "E03033",  27000,  "D101";
    "Kim Arlich",      "E10001",  57000,  "D190";
    "Timothy Grove",   "E16398",  29900,  "D190";
  ]

let topRank n =
  Seq.groupBy (fun (_, _, _, d) -> d) data
  |> Seq.map (snd >> Seq.sortBy (fun (_, _, s, _) -> -s) >> Seq.take n)
```



## Factor


```factor
USING: accessors assocs fry io kernel math.parser sequences
sorting ;
IN: top-rank

TUPLE: employee name id salary department ;

CONSTANT: employees {
        T{ employee f "Tyler Bennett" "E10297" 32000 "D101" }
        T{ employee f "John Rappl" "E21437" 47000 "D050" }
        T{ employee f "George Woltman" "E00127" 53500 "D101" }
        T{ employee f "Adam Smith" "E63535" 18000 "D202" }
        T{ employee f "Claire Buckman" "E39876" 27800 "D202" }
        T{ employee f "David McClellan" "E04242" 41500 "D101" }
        T{ employee f "Rich Holcomb" "E01234" 49500 "D202" }
        T{ employee f "Nathan Adams" "E41298" 21900 "D050" }
        T{ employee f "Richard Potter" "E43128" 15900 "D101" }
        T{ employee f "David Motsinger" "E27002" 19250 "D202" }
        T{ employee f "Tim Sampair" "E03033" 27000 "D101" }
        T{ employee f "Kim Arlich" "E10001" 57000 "D190" }
        T{ employee f "Timothy Grove" "E16398" 29900 "D190" }
    }

: group-by ( seq quot -- hash )
    H{ } clone [ '[ dup @ _ push-at ] each ] keep ; inline

: prepare-departments ( seq -- departments )
    [ department>> ] group-by
    [ [ salary>> ] inv-sort-with ] assoc-map ;

: first-n-each ( seq n quot -- )
    [ short head-slice ] dip each ; inline

: main ( -- )
    employees prepare-departments [
        [ "Department " write write ":" print ] dip
        3 [
            [ id>> write "  $" write ]
            [ salary>> number>string write "  " write ]
            [ name>> print ] tri
        ] first-n-each
        nl
    ] assoc-each ;
```
<pre style="height:30ex;overflow:scroll">
Department D101:
E00127  $53500  George Woltman
E04242  $41500  David McClellan
E10297  $32000  Tyler Bennett

Department D202:
E01234  $49500  Rich Holcomb
E39876  $27800  Claire Buckman
E27002  $19250  David Motsinger

Department D190:
E10001  $57000  Kim Arlich
E16398  $29900  Timothy Grove

Department D050:
E21437  $47000  John Rappl
E41298  $21900  Nathan Adams

```



## Forth


```Forth

\ Written in ANS-Forth; tested under VFX.
\ Requires the novice package: http://www.forth.org/novice.html
\ The following should already be done:
\ include novice.4th
\ include list.4th

marker TopRank.4th

\ This demonstrates how I typically use lists. A program such as this does not need any explicit iteration, so it is more like Factor than C.
\ I would define high-level languages as those that allow programs to be written without explicit iteration. Iteration is a major source of bugs.
\ The C library has QSORT that hides iteration, but user-written code very rarely uses this technique, and doesn't in the TopRank example.


\ ******
\ ****** The following defines our data-structure.
\ ****** Pretty much every struct definition has these basic functions.
\ ******

list
    w field .name       \ string
    w field .id         \ string
    w field .salary     \ integer
    w field .dept       \ string
constant employee

: init-employee ( name id salary department node -- node )
    init-list >r
    hstr    r@ .dept !
            r@ .salary !
    hstr    r@ .id !
    hstr    r@ .name !
    r> ;

: new-employee ( name id salary dept -- node )
    employee alloc
    init-employee ;

: <kill-employee> ( node -- )
    dup .name @     dealloc
    dup .id @       dealloc
    dup .dept @     dealloc
    dealloc ;

: kill-employee ( head -- )
    each[  <kill-employee>  ]each ;

: <clone-employee> ( node -- new-node )
    clone-node
    dup .name @     hstr    over .name !
    dup .id @       hstr    over .id !
    dup .dept @     hstr    over .dept ! ;

: clone-employee ( head -- new-head )
    nil
    swap each[  <clone-employee> link  ]each ;

: <show-employee> ( node -- )
    dup .id @       count type  4 spaces
    dup .dept @     count type  4 spaces
    dup .salary @   .           4 spaces
    dup .name @     count type  cr
    drop ;

: show-employee ( head -- )
    cr
    each[  <show-employee>  ]each ;


\ ******
\ ****** The following code is specific to the query that we want to do in this example problem.
\ ******

: employee-dept-salary ( new-node node -- new-node ? )                      \ for use by FIND-PRIOR or INSERT-ORDERED
    2dup
    .dept @ count   rot .dept @ count   compare     ?dup if  A>B =  nip exit then
    .salary @       over .salary @      < ;

: <top> ( n rank current-dept head node -- n rank current-dept head )
    2>r                         \ -- n rank current-dept
    dup count  r@ .dept @ count  compare  A=B <> if                         \ we have a new department
        2drop                                                               \ discard RANK and CURRENT-DEPT
        0  r@ .dept @  then                                                 \ start again with a new RANK and CURRENT-DEPT
    rover rover > if                                                        \ if N > RANK then it is good
        r> r>  over             \ -- n rank current-dept node head node
        <clone-employee> link   \ -- n rank current-dept node head
        >r >r  then             \ -- n rank current-dept
    swap 1+  swap                                                           \ increment RANK
    rdrop  r> ;                 \ -- n rank current-dept head

: top ( n head -- new-head )    \ make a new list of the top N salary earners in each dept      \ requires that list be sorted by dept-salary
    >r
    0  c" xxx"  nil             \ -- n rank current-dept new-head           \ initially for an invalid department
    r>  ['] <top>  each
    3nip ;


\ ******
\ ****** The following is a test of the program using sample data.
\ ******

nil  ' employee-dept-salary
c" Tyler Bennett"       c" E10297"  32000  c" D101"     new-employee  insert-ordered
c" John Rappl"          c" E21437"  47000  c" D050"     new-employee  insert-ordered
c" George Woltman"      c" E00127"  53500  c" D101"     new-employee  insert-ordered
c" Adam Smith"          c" E63535"  18000  c" D202"     new-employee  insert-ordered
c" Claire Buckman"      c" E39876"  27800  c" D202"     new-employee  insert-ordered
c" David McClellan"     c" E04242"  41500  c" D101"     new-employee  insert-ordered
c" Rich Holcomb"        c" E01234"  49500  c" D202"     new-employee  insert-ordered
c" Nathan Adams"        c" E41298"  21900  c" D050"     new-employee  insert-ordered
c" Richard Potter"      c" E43128"  15900  c" D101"     new-employee  insert-ordered
c" David Motsinger"     c" E27002"  19250  c" D202"     new-employee  insert-ordered
c" Tim Sampair"         c" E03033"  27000  c" D101"     new-employee  insert-ordered
c" Kim Arlich"          c" E10001"  57000  c" D190"     new-employee  insert-ordered
c" Timothy Grove"       c" E16398"  29900  c" D190"     new-employee  insert-ordered
drop  constant test-data

cr .( N = 0 )
0 test-data top  dup show-employee  kill-employee

cr .( N = 1 )
1 test-data top  dup show-employee  kill-employee

cr .( N = 2 )
2 test-data top  dup show-employee  kill-employee

cr .( N = 3 )
3 test-data top  dup show-employee  kill-employee

test-data kill-employee

```

<pre style="height:30ex;overflow:scroll">
N = 0

N = 1
E21437    D050    47000     John Rappl
E00127    D101    53500     George Woltman
E10001    D190    57000     Kim Arlich
E01234    D202    49500     Rich Holcomb

N = 2
E21437    D050    47000     John Rappl
E41298    D050    21900     Nathan Adams
E00127    D101    53500     George Woltman
E04242    D101    41500     David McClellan
E10001    D190    57000     Kim Arlich
E16398    D190    29900     Timothy Grove
E01234    D202    49500     Rich Holcomb
E39876    D202    27800     Claire Buckman

N = 3
E21437    D050    47000     John Rappl
E41298    D050    21900     Nathan Adams
E00127    D101    53500     George Woltman
E04242    D101    41500     David McClellan
E10297    D101    32000     Tyler Bennett
E10001    D190    57000     Kim Arlich
E16398    D190    29900     Timothy Grove
E01234    D202    49500     Rich Holcomb
E39876    D202    27800     Claire Buckman
E27002    D202    19250     David Motsinger

```



## Fortran

The example data can easily be declared via DATA statements, as with
```Fortran
      DATA EMPLOYEE(1:3)/
     1 GRIST("Tyler Bennett","E10297",32000,"D101"),
     2 GRIST("John Rappl","E21437",47000,"D050"),
     3 GRIST("George Woltman","E00127",53500,"D101")/

```

(just showing the first three), however this does not allow the preparation of the header line, because <code>GRIST("Employee Name","Employee ID","Salary","Department")</code> would be invalid since the "salary" entry is a floating-point number in the data aggregate, not text. The header line could be discarded, but this is in violation of the ideas of properly-described data files. So, the plan is to read the data from an input file containing the full example input, with its header line given special treatment. No attempt is made to detect or report improperly-formatted data. The READ statements could refer to an internally-defined block of text, but reading an actual disc file is more normal. Ad-hoc decisions are made for the size of the CHARACTER variables, that are "surely big enough" for the example data, likewise with the size of the array to hold all the records.

For simplicity, and following the specification to not parse the data at runtime but to use built-in facilities, free-format input is intended. This does not require text variables to be quoted, however, because ''both'' a comma ''and'' a space are accepted as delimiters the specified data cannot be read as provided since some text fields contain a space. Such text fields must be placed within quotes to avoid missteps. This of course must also be done if a DATA statement as above were to be employed. On the other hand, the example data contains only names with two parts separated by a space - and this could be relied on as a separator to read a two-part name variable. Similarly, all the salary values have the same number of digits and so could be sorted as text variables to give the desired numerical order. The resulting scheme will be easily disrupted by data other than those of the example! I recall working at the NZ Post Office in the 1970s when there came a time that higher-paid staff achieved fortnightly pays in excess of $999·99 and the computer system couldn't handle it. Similarly, I have a three-part name. Asian names are often written with family first, and there should be a comma when this ordering is used in English: Bloggs, Joe; Kim, Il-sung; Kim, Jong-il; Kim, Jong-un. To have such name texts read as one variable would definitely require them to be enclosed in quotes so that their included comma is not taken as a delimiter in free-format input, but a more likely usage would be to read two fields and argue over two-part names only. Some Asian names are one-part, even as a formal usage. The question is whether the names part is a "block" of text to be shown as-is, or whether it is to have components.

The only remaining requirement is that the type of datum being read matches the type of the corresponding variable in the ''input-list'' that is to receive that datum; happily this is the case as each record has four fields, always in the same order and the same type. (I have got nowhere explaining to a data supplier that the header line can use different names for the columns if they are always in the same order, or, the columns can be in a different order provided that their column names are always the same - but varying both the names and the order forces me to personally inspect the data file and make guesses. Bah.) Except for the header line, which has four character fields and so can be read into a CHARACTER array, as ever of a "suitable" size. There is alas no facility whereby, ''during the input processing itself'' the receiving variable can be resized to suit the actual size of its incoming datum, as it is being read. Fortran array indexing is done by calculating storage locations employing known and fixed size components. So, one must make ad-hoc choices that will work for the example, but which may run into trouble more generally. F90 supports facilities for allocating items of a size determined at run time, however this has to be done before the item is put to use, such as in a READ statement. But the input data are not to be parsed by special code, so reading an input line into a working area to determine the necessary sizes, allocating storage areas of suitable size, then transferring data from the scratchpad to the freshly-created recipients is disallowed here.

The source is unstructured by subroutines and the like, but uses the facilities of F90 whereby a compound data aggregate can be defined and used with a systematic nomenclature for the parts. Thus, array EMPLOYEE has components with suitable names. Previously, one would define separate simple arrays of the appropriate type, using a similar naming system in self-defence: perhaps ENAME, EID, ESAL, EDEPT or similar.

If the requirement had been to report the highest-ranked salary, the deed could have been done via the F90 function MAXLOC, which locates the array index of the (first-encountered?) maximum value of an array of values; further, it allows an additional MASK parameter which could be used to select only those entries having a particular "department" code. However, aside from the multiple scans that would be required for multiple departmental codes, there is the requirement for the top N salaries, and anyway, there would still be the need to identify the different departmental codes once only. Accordingly, the way ahead is the classic: sort the data and then work through the sorted sequence. Since standard Fortran provides no "sort" function in its standard libraries nor does it offer a pre-processor that might include a sort generator and the like to generate the specific code needed for a particular data aggregate and sort key specification, one must engage in some repetition. For this purpose, the "comb" sort is convenient with its code requiring just one comparison and one "swap" action, so in-line code is not troublesome. On the other hand, when dealing with compound sort keys, a three-way IF statement (or equivalent) is necessary whereby the first field of the sort key is compared with appropriate action for the < and > cases, but for the = case the comparison moves on to the second key. ''Three'' different consequences from the one comparison. Despite the deprecations of the modernisers this is still possible for numerical comparisons, but alas, character comparisons are not allowed in an arithmetic-IF statement - though one could struggle with ICHAR action. Thus, two comparisons are made where only one should suffice.

Finally, standard Fortran does not offer a means of receiving parameters as might be supplied when a code file is activated in a command-line environment. There may be special library routines with names such as GETARG, but they're not standard. So, a READ statement is employed. Or, one could rewrite this routine as a subroutine having one parameter so as to approximate the form of the specification more closely. Extra statements...
```Fortran
      PROGRAM TOP RANK	!Just do it.
      CHARACTER*28 HEADING(4)	!The first line is a header.
      TYPE GRIST		!But this is the stuff.
       CHARACTER*28 NAME		!Arbitrary sizes.
       CHARACTER*6 ID			!Possibly imperfect.
       REAL*8 SALARY			!Single precision is a bit thin.
       CHARACTER*6 DEPARTMENT		!Not a number.
      END TYPE GRIST		!So much for the aggregate.
      INTEGER HORDE		!Some parameterisation.
      PARAMETER (HORDE = 66)	!This should suffice.
      TYPE(GRIST) EMPLOYEE(HORDE),HIC	!An extra for the sort.
      LOGICAL CURSE		!Possible early completion.
      INTEGER I,N,H		!Steppers.
      INTEGER II,R		!Needed for the results.
      INTEGER KBD,MSG,IN	!I/O unit numbers.

      KBD = 5	!Standard input.
      MSG = 6	!Standard output.
      IN = 10	!Suitable for an input file.
      WRITE (MSG,1)
    1 FORMAT ("Reads a set of employee information from TopRank.csv"/
     1"Then for each department code, shows the N highest paid.")
      OPEN (IN,NAME = "TopRank.csv",FORM = "FORMATTED")
      READ (IN,*) HEADING	!Column headings: the "Salary" heading is not numeric.
Chug through the input.
      N = 0		!None so far.
   10 READ (IN,*,END = 20) EMPLOYEE(N + 1)	!Get the next record.
      N = N + 1					!We did. Count it in.
      IF (N.GT.HORDE) STOP "Too many employee records!"	!Ah, suspicion.
      GO TO 10					!Perhaps there will be another.
Collate the collection.
   20 CLOSE(IN)			!Finished with the input.
Crank up a comb sort, which requires only one comparison statement. Especially good for compound fields.
      H = N - 1			!Last - first, and not + 1.
   21 H = MAX(1,H*10/13)	!The special feature.
      CURSE = .FALSE.		!So far, so good.
      DO I = N - H,1,-1		!If H = 1, this is a Bubblesort.
        IF (EMPLOYEE(I).DEPARTMENT.LT.EMPLOYEE(I + H).DEPARTMENT) CYCLE	!In order by department.
        IF (EMPLOYEE(I).DEPARTMENT.EQ.EMPLOYEE(I + H).DEPARTMENT	!Or, Equal department,
     *  .AND. EMPLOYEE(I).SALARY.GT.EMPLOYEE(I + H).SALARY) CYCLE	!And in decreasing order by salary.
        CURSE = .TRUE.			!No escape. the elements are in the wrong order.
        HIC = EMPLOYEE(I)		!So a SWAP statement would be good. But alas.
        EMPLOYEE(I) = EMPLOYEE(I + H)	!For large data aggregates, an indexed sort would be good.
        EMPLOYEE(I + H) = HIC		!But, just slog away.
      END DO				!Consider the next pairing.
      IF (CURSE .OR. H.GT.1) GO TO 21	!Work remains?

Cast forth results.
   30 WRITE (6,31) N	!Announce, and solicit a parameter.
   31 FORMAT (I0," employees. How many per dept? ",$)	!The $ sez "don't start a new line."
      READ (KBD,*) R	!The parameter.
      IF (R.LE.0) STOP	!bah.
      WRITE (MSG,32) "Rank",HEADING
   32 FORMAT (/,A6,1X,A28,2X,A12,1X,A10,A)	!Compare to FORMAT 33.
      HIC.DEPARTMENT = "...Not this"	!Different from all departmental codes.
      DO I = 1,N	!Scan the sorted data.
        IF (HIC.DEPARTMENT.EQ.EMPLOYEE(I).DEPARTMENT) THEN	!Another?
          II = II + 1		!Same department, so count another adherent.
         ELSE			!But with a change of department code,
          II = 1		!Start a fresh count.
          HIC.DEPARTMENT = EMPLOYEE(I).DEPARTMENT	!And remember the new code.
          WRITE (MSG,*) "For ",HIC.DEPARTMENT	!Announce the new department's code.
        END IF			!So much for grouping.
        IF (II.LE.R) WRITE (MSG,33) II,EMPLOYEE(I)	!Still within the desired rank?
   33   FORMAT (I6,1X,A28,1X,A12,F11.2,1X,A)	!Some layout. Includes the repeated departmental code name.
      END DO			!On to the next.

      END	!That was straightforward.

```


As ever, some tricky sizing of fields in the FORMAT statements, and a collection of ad-hoc constants. Cross-linking the integers via PARAMETER statements and the like would add a whole lot of blather, so if field sizes are changed, a fair amount of fiddling would follow. A more accomplished data processing routine would include this.

Output:

```txt

Reads a set of employee information from TopRank.csv
Then for each department code, shows the N highest paid.
13 employees. How many per dept? 3

  Rank Employee Name                 Employee ID  Salary    Department
 For D050
     1 John Rappl                         E21437   47000.00 D050
     2 Nathan Adams                       E41298   21900.00 D050
 For D101
     1 George Woltman                     E00127   53500.00 D101
     2 David McClellan                    E04242   41500.00 D101
     3 Tyler Bennett                      E10297   32000.00 D101
 For D190
     1 Kim Arlich                         E10001   57000.00 D190
     2 Timothy Grove                      E16398   29900.00 D190
 For D202
     1 Rich Holcomb                       E01234   49500.00 D202
     2 Claire Buckman                     E39876   27800.00 D202
     3 David Motsinger                    E27002   19250.00 D202

```



## FunL


```funl
data Employee( name, id, salary, dept )

employees = [
  Employee( 'Tyler Bennett', 'E10297', 32000, 'D101' ),
  Employee( 'John Rappl', 'E21437', 47000, 'D050' ),
  Employee( 'George Woltman', 'E00127', 53500, 'D101' ),
  Employee( 'Adam Smith', 'E63535', 18000, 'D202' ),
  Employee( 'Claire Buckman', 'E39876', 27800, 'D202' ),
  Employee( 'David McClellan', 'E04242', 41500, 'D101' ),
  Employee( 'Rich Holcomb', 'E01234', 49500, 'D202' ),
  Employee( 'Nathan Adams', 'E41298', 21900, 'D050' ),
  Employee( 'Richard Potter', 'E43128', 15900, 'D101' ),
  Employee( 'David Motsinger', 'E27002', 19250, 'D202' ),
  Employee( 'Tim Sampair', 'E03033', 27000, 'D101' ),
  Employee( 'Kim Arlich', 'E10001', 57000, 'D190' ),
  Employee( 'Timothy Grove', 'E16398', 29900, 'D190' )
  ]

N = 2

for (dept, empl) <- employees.groupBy( e -> e.dept ).>toList().sortWith( (<) )
  println( dept )

  for e <- empl.sortWith( \a, b -> a.salary > b.salary ).take( N )
    printf( "    %-16s  %6s  %7d\n", e.name, e.id, e.salary )

  println()
```


```txt

D050
    John Rappl        E21437    47000
    Nathan Adams      E41298    21900

D101
    George Woltman    E00127    53500
    David McClellan   E04242    41500

D190
    Kim Arlich        E10001    57000
    Timothy Grove     E16398    29900

D202
    Rich Holcomb      E01234    49500
    Claire Buckman    E39876    27800

```



## Go


```go
package main

import (
	"fmt"
	"sort"
)

// language-native data description
type Employee struct {
	Name, ID string
	Salary   int
	Dept     string
}

type EmployeeList []*Employee

var data = EmployeeList{
	{"Tyler Bennett", "E10297", 32000, "D101"},
	{"John Rappl", "E21437", 47000, "D050"},
	{"George Woltman", "E00127", 53500, "D101"},
	{"Adam Smith", "E63535", 18000, "D202"},
	{"Claire Buckman", "E39876", 27800, "D202"},
	{"David McClellan", "E04242", 41500, "D101"},
	{"Rich Holcomb", "E01234", 49500, "D202"},
	{"Nathan Adams", "E41298", 21900, "D050"},
	{"Richard Potter", "E43128", 15900, "D101"},
	{"David Motsinger", "E27002", 19250, "D202"},
	{"Tim Sampair", "E03033", 27000, "D101"},
	{"Kim Arlich", "E10001", 57000, "D190"},
	{"Timothy Grove", "E16398", 29900, "D190"},
	// Extra data to demonstrate ties
	{"Tie A", "E16399", 29900, "D190"},
	{"Tie B", "E16400", 29900, "D190"},
	{"No Tie", "E16401", 29899, "D190"},
}

// We only need one type of ordering/grouping for this task so we could directly
// implement sort.Interface on EmployeeList (or a byDeptSalary alias type) with
// the appropriate Less method.
//
// Instead, we'll add a bit here that makes it easier to use arbitrary orderings.
// This is like the "SortKeys" Planet sorting example in the sort package
// documentation, see https://golang.org/pkg/sort

type By func(e1, e2 *Employee) bool
type employeeSorter struct {
	list EmployeeList
	by   func(e1, e2 *Employee) bool
}

func (by By) Sort(list EmployeeList)         { sort.Sort(&employeeSorter{list, by}) }
func (s *employeeSorter) Len() int           { return len(s.list) }
func (s *employeeSorter) Swap(i, j int)      { s.list[i], s.list[j] = s.list[j], s.list[i] }
func (s *employeeSorter) Less(i, j int) bool { return s.by(s.list[i], s.list[j]) }

// For this specific task we could just write the data to an io.Writer
// but in general it's better to return the data in a usable form (for
// example, perhaps other code want's to do something like compare the
// averages of the top N by department).
//
// So we go through the extra effort of returning an []EmployeeList, a
// list of employee lists, one per deparment. The lists are trimmed to
// to the top 'n', which can be larger than n if there are ties for the
// nth salary (callers that don't care about ties could just trim more.)
func (el EmployeeList) TopSalariesByDept(n int) []EmployeeList {
	if n <= 0 || len(el) == 0 {
		return nil
	}
	deptSalary := func(e1, e2 *Employee) bool {
		if e1.Dept != e2.Dept {
			return e1.Dept < e2.Dept
		}
		if e1.Salary != e2.Salary {
			return e1.Salary > e2.Salary
		}
		// Always have some unique field as the last one in a sort list
		return e1.ID < e2.ID
	}

	// We could just sort the data in place for this task. But
	// perhaps messing with the order is undesirable or there is
	// other concurrent access. So we'll make a copy and sort that.
	// It's just pointers so the amount to copy is relatively small.
	sorted := make(EmployeeList, len(el))
	copy(sorted, el)
	By(deptSalary).Sort(sorted)

	perDept := []EmployeeList{}
	var lastDept string
	var lastSalary int
	for _, e := range sorted {
		if e.Dept != lastDept || len(perDept) == 0 {
			lastDept = e.Dept
			perDept = append(perDept, EmployeeList{e})
		} else {
			i := len(perDept) - 1
			if len(perDept[i]) >= n && e.Salary != lastSalary {
				continue
			}
			perDept[i] = append(perDept[i], e)
			lastSalary = e.Salary
		}
	}
	return perDept
}

func main() {
	const n = 3
	top := data.TopSalariesByDept(n)
	if len(top) == 0 {
		fmt.Println("Nothing to show.")
		return
	}
	fmt.Printf("Top %d salaries per department\n", n)
	for _, list := range top {
		fmt.Println(list[0].Dept)
		for _, e := range list {
			fmt.Printf("    %s %16s %7d\n", e.ID, e.Name, e.Salary)
		}
	}
}
```

<pre style="height:30ex;overflow:scroll">
Top 3 salaries per department
D050
    E21437       John Rappl   47000
    E41298     Nathan Adams   21900
D101
    E00127   George Woltman   53500
    E04242  David McClellan   41500
    E10297    Tyler Bennett   32000
D190
    E10001       Kim Arlich   57000
    E16398    Timothy Grove   29900
    E16399            Tie A   29900
    E16400            Tie B   29900
D202
    E01234     Rich Holcomb   49500
    E39876   Claire Buckman   27800
    E27002  David Motsinger   19250

```



## Groovy


```groovy
def displayRank(employees, number) {
    employees.groupBy { it.Department }.sort().each { department, staff ->
        println "Department $department"
        println "    Name                ID      Salary"
        staff.sort { e1, e2 -> e2.Salary <=> e1.Salary }
        staff[0..<Math.min(number, staff.size())].each { e ->
           println "    ${e.Name.padRight(20)}${e.ID}${sprintf('%8d', e.Salary)}"
        }
        println()
    }
}

def employees = [
        [Name: 'Tyler Bennett', ID: 'E10297', Salary: 32000, Department: 'D101'],
        [Name: 'John Rappl', ID: 'E21437', Salary: 47000, Department: 'D050'],
        [Name: 'George Woltman', ID: 'E00127', Salary: 53500, Department: 'D101'],
        [Name: 'Adam Smith', ID: 'E63535', Salary: 18000, Department: 'D202'],
        [Name: 'Claire Buckman', ID: 'E39876', Salary: 27800, Department: 'D202'],
        [Name: 'David McClellan', ID: 'E04242', Salary: 41500, Department: 'D101'],
        [Name: 'Rich Holcomb', ID: 'E01234', Salary: 49500, Department: 'D202'],
        [Name: 'Nathan Adams', ID: 'E41298', Salary: 21900, Department: 'D050'],
        [Name: 'Richard Potter', ID: 'E43128', Salary: 15900, Department: 'D101'],
        [Name: 'David Motsinger', ID: 'E27002', Salary: 19250, Department: 'D202'],
        [Name: 'Tim Sampair', ID: 'E03033', Salary: 27000, Department: 'D101'],
        [Name: 'Kim Arlich', ID: 'E10001', Salary: 57000, Department: 'D190'],
        [Name: 'Timothy Grove', ID: 'E16398', Salary: 29900, Department: 'D190']
]
displayRank(employees, 3)
```
<pre style="height:30ex;overflow:scroll">Department D050
    Name                ID      Salary
    John Rappl          E21437   47000
    Nathan Adams        E41298   21900

Department D101
    Name                ID      Salary
    George Woltman      E00127   53500
    David McClellan     E04242   41500
    Tyler Bennett       E10297   32000

Department D190
    Name                ID      Salary
    Kim Arlich          E10001   57000
    Timothy Grove       E16398   29900

Department D202
    Name                ID      Salary
    Rich Holcomb        E01234   49500
    Claire Buckman      E39876   27800
    David Motsinger     E27002   19250
```



## Haskell


### =Data.List=

```haskell
import Data.List (sortBy, groupBy)

import Text.Printf (printf)

import Data.Ord (comparing)

import Data.Function (on)

type ID = Int

type DEP = String

type NAME = String

type SALARY = Double

data Employee = Employee
  { nr :: ID
  , dep :: DEP
  , name :: NAME
  , sal :: SALARY
  }

employees :: [Employee]
employees =
  fmap
    (\(i, d, n, s) -> Employee i d n s)
    [ (1001, "AB", "Janssen A.H.", 41000)
    , (101, "KA", "'t Woud B.", 45000)
    , (1013, "AB", "de Bont C.A.", 65000)
    , (1101, "CC", "Modaal A.M.J.", 30000)
    , (1203, "AB", "Anders H.", 50000)
    , (100, "KA", "Ezelbips P.J.", 52000)
    , (1102, "CC", "Zagt A.", 33000)
    , (1103, "CC", "Ternood T.R.", 21000)
    , (1104, "CC", "Lageln M.", 23000)
    , (1105, "CC", "Amperwat A.", 19000)
    , (1106, "CC", "Boon T.J.", 25000)
    , (1107, "CC", "Beloop L.O.", 31000)
    , (1009, "CD", "Janszoon A.", 38665)
    , (1026, "CD", "Janszen H.P.", 41000)
    , (1011, "CC", "de Goeij J.", 39000)
    , (106, "KA", "Pragtweik J.M.V.", 42300)
    , (111, "KA", "Bakeuro S.", 31000)
    , (105, "KA", "Clubdrager C.", 39800)
    , (104, "KA", "Karendijk F.", 23000)
    , (107, "KA", "Centjes R.M.", 34000)
    , (119, "KA", "Tegenstroom H.L.", 39000)
    , (1111, "CD", "Telmans R.M.", 55500)
    , (1093, "AB", "de Slegte S.", 46987)
    , (1199, "CC", "Uitlaat G.A.S.", 44500)
    ]

firstN :: Int
       -> (Employee -> DEP)
       -> (Employee -> SALARY)
       -> [Employee]
       -> [[Employee]]
firstN n o1 o2 x =
  fmap
    (take n . sortBy (comparingDown o2))
    (groupBy (groupingOn o1) (sortBy (comparing o1) x))

groupingOn
  :: Eq a1
  => (a -> a1) -> a -> a -> Bool
groupingOn = ((==) `on`)

comparingDown
  :: Ord a
  => (b -> a) -> b -> b -> Ordering
comparingDown = flip . comparing

main :: IO ()
main = do
  printf "%-16s %3s %10s\n" "NAME" "DEP" "TIP"
  putStrLn $ replicate 31 '='
  mapM_ (traverse ((printf "%-16s %3s %10.2g\n" . name) <*> dep <*> sal)) $
    firstN 3 dep sal employees
```

```txt
NAME             DEP        TIP

### =========================

de Bont C.A.      AB   65000.00
Anders H.         AB   50000.00
de Slegte S.      AB   46987.00
Uitlaat G.A.S.    CC   44500.00
de Goeij J.       CC   39000.00
Zagt A.           CC   33000.00
Telmans R.M.      CD   55500.00
Janszen H.P.      CD   41000.00
Janszoon A.       CD   38665.00
Ezelbips P.J.     KA   52000.00
't Woud B.        KA   45000.00
Pragtweik J.M.V.  KA   42300.00
```



### =Data.Map=

Alternatively, if we store the data in key-value maps, rather than in a cons list, we can use Map.lookup and Map.filter, Map.keys and Map.elems, to pull out the data and shape reports fairly flexibly.


```Haskell
import qualified Data.Map as M

import Data.Ord (comparing)
import Control.Monad (join)
import Data.Maybe (fromJust)
import Data.List (nub, sortBy, sort, intercalate, transpose)

mapNames, mapDepts :: M.Map Int String
[mapNames, mapDepts] = pure readPairs <*> [nameKV, deptKV] <*> [xs]

mapSalaries :: M.Map Int Int
mapSalaries = readPairs salaryKV xs

highSalaryKeys :: Int -> String -> [(Int, Int)]
highSalaryKeys n dept =
  take n $
  sortBy ((flip . comparing) snd) $
  (,) <*> (fromJust . flip M.lookup mapSalaries) <$>
  M.keys (M.filter (== dept) mapDepts)

reportLines :: String -> [(Int, Int)] -> [[String]]
reportLines dept =
  fmap (\(k, n) -> [fromJust $ M.lookup k mapNames, dept, show n])

xs :: [(Int, String, String, Int)]
xs =
  [ (1001, "AB", "Janssen A.H.", 41000)
  , (101, "KA", "'t Woud B.", 45000)
  , (1013, "AB", "de Bont C.A.", 65000)
  , (1101, "CC", "Modaal A.M.J.", 30000)
  , (1203, "AB", "Anders H.", 50000)
  , (100, "KA", "Ezelbips P.J.", 52000)
  , (1102, "CC", "Zagt A.", 33000)
  , (1103, "CC", "Ternood T.R.", 21000)
  , (1104, "CC", "Lageln M.", 23000)
  , (1105, "CC", "Amperwat A.", 19000)
  , (1106, "CC", "Boon T.J.", 25000)
  , (1107, "CC", "Beloop L.O.", 31000)
  , (1009, "CD", "Janszoon A.", 38665)
  , (1026, "CD", "Janszen H.P.", 41000)
  , (1011, "CC", "de Goeij J.", 39000)
  , (106, "KA", "Pragtweik J.M.V.", 42300)
  , (111, "KA", "Bakeuro S.", 31000)
  , (105, "KA", "Clubdrager C.", 39800)
  , (104, "KA", "Karendijk F.", 23000)
  , (107, "KA", "Centjes R.M.", 34000)
  , (119, "KA", "Tegenstroom H.L.", 39000)
  , (1111, "CD", "Telmans R.M.", 55500)
  , (1093, "AB", "de Slegte S.", 46987)
  , (1199, "CC", "Uitlaat G.A.S.", 44500)
  ]

readPairs
  :: Ord k
  => (a1 -> (k, a)) -> [a1] -> M.Map k a
readPairs f xs = M.fromList $ f <$> xs

nameKV, deptKV :: (Int, String, String, Int) -> (Int, String)
nameKV (k, _, name, _) = (k, name)

deptKV (k, dept, _, _) = (k, dept)

salaryKV :: (Int, String, String, Int) -> (Int, Int)
salaryKV (k, _, _, salary) = (k, salary)

table :: String -> [[String]] -> [String]
table delim rows =
  let justifyLeft c n s = take n (s ++ replicate n c)
      justifyRight c n s = drop (length s) (replicate n c ++ s)
  in intercalate delim <$>
     transpose
       ((fmap =<< justifyLeft ' ' . maximum . fmap length) <$> transpose rows)

main :: IO ()
main =
  (putStrLn . unlines)
    (table
       "   "
       (join
          (reportLines <*> highSalaryKeys 3 <$> (sort . nub) (M.elems mapDepts))))
```

```txt
de Bont C.A.       AB   65000
Anders H.          AB   50000
de Slegte S.       AB   46987
Uitlaat G.A.S.     CC   44500
de Goeij J.        CC   39000
Zagt A.            CC   33000
Telmans R.M.       CD   55500
Janszen H.P.       CD   41000
Janszoon A.        CD   38665
Ezelbips P.J.      KA   52000
't Woud B.         KA   45000
Pragtweik J.M.V.   KA   42300
```



## HicEst


```HicEst
CHARACTER source="Test.txt", outP='Top_rank.txt', fmt='A20,A8,i6,2x,A10'
CHARACTER name*20, employee_ID*10, department*10, temp*10
REAL ::   idx(1), N_top_salaries=3

! Open the source with 4 "," separated columns, skip line 1:
OPEN(FIle=source, Format='SL=1;4,;', LENgth=L)
ALLOCATE(idx, L)

! Sort salary column 3 descending, then department column 4, store in idx:
SORT(FIle=source, Column=3, Descending=1, Column=4, Index=idx)

! Display a spreadsheet-like scrolling dialog of the presorted source:
DLG(Text=idx, Text=source, Format=fmt, Y)

! Output the first N top salaries of each department_
OPEN(FIle=outP)
DO i = 1, L
   rank = rank + 1
   READ(FIle=source, Row=idx(i)) name, employee_ID, salary, department
   IF(temp /= department) THEN
       rank = 1
       WRITE(FIle=outP)
       temp = department
   ENDIF
   IF(rank <= N_top_salaries) THEN
       WRITE(FIle=outP, Format=fmt) name, employee_ID, salary, department
   ENDIF
ENDDO

END
```
<pre style="height:30ex;overflow:scroll">John Rappl          E21437   47000  D050
Nathan Adams        E41298   21900  D050

George Woltman      E00127   53500  D101
David McClellan     E04242   41500  D101
Tyler Bennett       E10297   32000  D101

Kim Arlich          E10001   57000  D190
Timothy Grove       E16398   29900  D190

Rich Holcomb        E01234   49500  D202
Claire Buckman      E39876   27800  D202
David Motsinger     E27002   19250  D202
```

Note: In place of writing the sorted result to file outP, [http://www.HicEst.com/DLG.htm DLG] allows the direct export of the formatted and sorted result.
Use of the CLUSter= and Groups= options of [http://www.HicEst.com/SORT.htm SORT] performs a variance controlled cluster sort of up to 8 columns instead of the department step sort in this example.

=={{header|Icon}} and {{header|Unicon}}==

```icon
record Employee(name,id,salary,dept)

procedure getEmployees ()
  employees := [
    Employee("Tyler Bennett","E10297",32000,"D101"),
    Employee("John Rappl","E21437",47000,"D050"),
    Employee("George Woltman","E00127",53500,"D101"),
    Employee("Adam Smith","E63535",18000,"D202"),
    Employee("Claire Buckman","E39876",27800,"D202"),
    Employee("David McClellan","E04242",41500,"D101"),
    Employee("Rich Holcomb","E01234",49500,"D202"),
    Employee("Nathan Adams","E41298",21900,"D050"),
    Employee("Richard Potter","E43128",15900,"D101"),
    Employee("David Motsinger","E27002",19250,"D202"),
    Employee("Tim Sampair","E03033",27000,"D101"),
    Employee("Kim Arlich","E10001",57000,"D190"),
    Employee("Timothy Grove","E16398",29900,"D190")
  ]
  return employees
end

procedure show_employee (employee)
  every writes (!employee || " ")
  write ()
end

procedure main (args)
  N := integer(args[1]) # N set from command line
  employees := getEmployees ()
  groups := set()
  every employee := !employees do insert(groups, employee.dept)

  every group := !groups do {
    write ("== Top " || N || " in group " || group)
    employeesInGroup := []
    every employee := !employees do {
      if employee.dept == group then put(employeesInGroup, employee)
    }
    # sort by third field and reverse, so highest salary comes first
    employeesInGroup := reverse(sortf(employeesInGroup, 3))
    # show the first N records, up to the end of the list
    every show_employee (!employeesInGroup[1:(1+min(N,*employeesInGroup))])
  }
end
```
<pre style="height:30ex;overflow:scroll">
$ ./top-rank-by-group 2
== Top 2 in group D101
George Woltman E00127 53500 D101
David McClellan E04242 41500 D101
== Top 2 in group D202
Rich Holcomb E01234 49500 D202
Claire Buckman E39876 27800 D202
== Top 2 in group D050
John Rappl E21437 47000 D050
Nathan Adams E41298 21900 D050
== Top 2 in group D190
Kim Arlich E10001 57000 D190
Timothy Grove E16398 29900 D190

$ ./top-rank-by-group 4
== Top 4 in group D101
George Woltman E00127 53500 D101
David McClellan E04242 41500 D101
Tyler Bennett E10297 32000 D101
Tim Sampair E03033 27000 D101
== Top 4 in group D202
Rich Holcomb E01234 49500 D202
Claire Buckman E39876 27800 D202
David Motsinger E27002 19250 D202
Adam Smith E63535 18000 D202
== Top 4 in group D050
John Rappl E21437 47000 D050
Nathan Adams E41298 21900 D050
== Top 4 in group D190
Kim Arlich E10001 57000 D190
Timothy Grove E16398 29900 D190
```



## J

J has a rich set of primitive functions, which combine the power of an imperative language with the expressiveness of a declarative, SQL-like language:


```j
NB.  Dynamically generate convenience functions
('`',,;:^:_1: N=:{.Employees) =:, (_&{"1)`'' ([^:(_ -: ])L:0)"0 _~ i.# E =: {: Employees

NB.  Show top six ranked employees in each dept
N , (<@:>"1@:|:@:((6 <. #) {. ] \: SALARY)/.~ DEPT) |: <"1&> E
```

<pre style="height:30ex;overflow:scroll">
 +-----+-----+-----------------+------+
 |ID   |DEPT |NAME             |SALARY|
 +-----+-----+-----------------+------+
 |1013 |AB   |de Bont C.A.     |65000 |
 |1203 |AB   |Anders H.        |50000 |
 |1093 |AB   |de Slegte S.     |46987 |
 |1001 |AB   |Janssen A.H.     |41000 |
 +-----+-----+-----------------+------+
 |100  |KA   |Ezelbips P.J.    |52000 |
 |101  |KA   |'t Woud B.       |45000 |
 |106  |KA   |Pragtweik J.M.V. |42300 |
 |105  |KA   |Clubdrager C.    |39800 |
 |119  |KA   |Tegenstroom H.L. |39000 |
 |107  |KA   |Centjes R.M.     |34000 |
 +-----+-----+-----------------+------+
 |1199 |CC   |Uitlaat G.A.S.   |44500 |
 |1011 |CC   |de Goeij J.      |39000 |
 |1102 |CC   |Zagt A.          |33000 |
 |1107 |CC   |Beloop L.O.      |31000 |
 |1101 |CC   |Modaal A.M.J.    |30000 |
 |1106 |CC   |Boon T.J.        |25000 |
 +-----+-----+-----------------+------+
 |1111 |CD   |Telmans R.M.     |55500 |
 |1026 |CD   |Janszen H.P.     |41000 |
 |1009 |CD   |Janszoon A.      |38665 |
 +-----+-----+-----------------+------+
```


using the data set:<pre style="height:30ex;overflow:scroll">
    Employees=: (<;.1~(1 1{.~#);+./@:(;:E.S:0])@:{.)];._2 noun define
    ID   DEPT NAME             SALARY
    1001 AB   Janssen A.H.     41000
    101  KA   't Woud B.       45000
    1013 AB   de Bont C.A.     65000
    1101 CC   Modaal A.M.J.    30000
    1203 AB   Anders H.        50000
    100  KA   Ezelbips P.J.    52000
    1102 CC   Zagt A.          33000
    1103 CC   Ternood T.R.     21000
    1104 CC   Lageln M.        23000
    1105 CC   Amperwat A.      19000
    1106 CC   Boon T.J.        25000
    1107 CC   Beloop L.O.      31000
    1009 CD   Janszoon A.      38665
    1026 CD   Janszen H.P.     41000
    1011 CC   de Goeij J.      39000
    106  KA   Pragtweik J.M.V. 42300
    111  KA   Bakeuro S.       31000
    105  KA   Clubdrager C.    39800
    104  KA   Karendijk F.     23000
    107  KA   Centjes R.M.     34000
    119  KA   Tegenstroom H.L. 39000
    1111 CD   Telmans R.M.     55500
    1093 AB   de Slegte S.     46987
    1199 CC   Uitlaat G.A.S.   44500
    )
```


Named as a function where the (maximum) number of employees in each department is a parameter:


```j
reportTopSalaries=: 3 :'N , (<@:>"1@:|:@:((y <. #) {. ] \: SALARY)/.~ DEPT) |: <"1&> E'
```
<pre style="height:30ex;overflow:scroll">
    reportTopSalaries 2
 +-----+-----+-----------------+------+
 |ID   |DEPT |NAME             |SALARY|
 +-----+-----+-----------------+------+
 |1013 |AB   |de Bont C.A.     |65000 |
 |1203 |AB   |Anders H.        |50000 |
 +-----+-----+-----------------+------+
 |100  |KA   |Ezelbips P.J.    |52000 |
 |101  |KA   |'t Woud B.       |45000 |
 +-----+-----+-----------------+------+
 |1199 |CC   |Uitlaat G.A.S.   |44500 |
 |1011 |CC   |de Goeij J.      |39000 |
 +-----+-----+-----------------+------+
 |1111 |CD   |Telmans R.M.     |55500 |
 |1026 |CD   |Janszen H.P.     |41000 |
 +-----+-----+-----------------+------+
```



## Java

```java
import java.io.File;
import java.util.*;

public class TopRankPerGroup {

    private static class Employee {
        final String name;
        final String id;
        final String department;
        final int salary;

        Employee(String[] rec) {
            name = rec[0];
            id = rec[1];
            salary = Integer.parseInt(rec[2]);
            department = rec[3];
        }

        @Override
        public String toString() {
            return String.format("%s %s %d %s", id, name, salary, department);
        }
    }

    public static void main(String[] args) throws Exception {
        int N = args.length > 0 ? Integer.parseInt(args[0]) : 3;

        Map<String, List<Employee>> records = new TreeMap<>();
        try (Scanner sc = new Scanner(new File("data.txt"))) {
            while (sc.hasNextLine()) {
                String[] rec = sc.nextLine().trim().split(", ");

                List<Employee> lst = records.get(rec[3]);
                if (lst == null) {
                    lst = new ArrayList<>();
                    records.put(rec[3], lst);
                }
                lst.add(new Employee(rec));
            }
        }

        records.forEach((key, val) -> {
            System.out.printf("%nDepartment %s%n", key);
            val.stream()
                .sorted((a, b) -> Integer.compare(b.salary, a.salary))
                .limit(N).forEach(System.out::println);
        });
    }
}
```


<pre style="height:30ex;overflow:scroll">Department D050
E21437 John Rappl 47000 D050
E41298 Nathan Adams 21900 D050

Department D101
E00127 George Woltman 53500 D101
E04242 David McClellan 41500 D101
E10297 Tyler Bennett 32000 D101

Department D190
E10001 Kim Arlich 57000 D190
E16398 Timothy Grove 29900 D190

Department D202
E01234 Rich Holcomb 49500 D202
E39876 Claire Buckman 27800 D202
E27002 David Motsinger 19250 D202
```




## JavaScript



### Iterative Solution


```javascript
var data = [
    {name: "Tyler Bennett",   id: "E10297", salary: 32000, dept: "D101"},
    {name: "John Rappl",      id: "E21437", salary: 47000, dept: "D050"},
    {name: "George Woltman",  id: "E00127", salary: 53500, dept: "D101"},
    {name: "Adam Smith",      id: "E63535", salary: 18000, dept: "D202"},
    {name: "Claire Buckman",  id: "E39876", salary: 27800, dept: "D202"},
    {name: "David McClellan", id: "E04242", salary: 41500, dept: "D101"},
    {name: "Rich Holcomb",    id: "E01234", salary: 49500, dept: "D202"},
    {name: "Nathan Adams",    id: "E41298", salary: 21900, dept: "D050"},
    {name: "Richard Potter",  id: "E43128", salary: 15900, dept: "D101"},
    {name: "David Motsinger", id: "E27002", salary: 19250, dept: "D202"},
    {name: "Tim Sampair",     id: "E03033", salary: 27000, dept: "D101"},
    {name: "Kim Arlich",      id: "E10001", salary: 57000, dept: "D190"},
    {name: "Timothy Grove",   id: "E16398", salary: 29900, dept: "D190"},
];

function top_rank(n) {
    var by_dept = group_by_dept(data);
    for (var dept in by_dept) {
        output(dept);
        for (var i = 0; i < n && i < by_dept[dept].length; i++) {
            var emp = by_dept[dept][i];
            output(emp.name + ", id=" + emp.id + ", salary=" + emp.salary);
        }
        output("");
    }
}

// group by dept, and sort by salary
function group_by_dept(data) {
    var by_dept = {};
    for (var idx in data)  {
        var dept = data[idx].dept;
        if ( ! has_property(by_dept, dept)) {
            by_dept[dept] = new Array();
        }
        by_dept[dept].push(data[idx]);
    }
    for (var dept in by_dept) {
        // numeric sort
        by_dept[dept].sort(function (a,b){return b.salary - a.salary});
    }
    return by_dept;
}

function has_property(obj, propname) {
    return typeof(obj[propname]) != "undefined";
}

function output(str) {
    try {
        WScript.Echo(str);  // WSH
    } catch(err) {
        print(str);  // Rhino
    }
}

top_rank(3);

```

<pre style="height:30ex;overflow:scroll">D101
George Woltman, id=E00127, salary=53500
David McClellan, id=E04242, salary=41500
Tyler Bennett, id=E10297, salary=32000

D050
John Rappl, id=E21437, salary=47000
Nathan Adams, id=E41298, salary=21900

D202
Rich Holcomb, id=E01234, salary=49500
Claire Buckman, id=E39876, salary=27800
David Motsinger, id=E27002, salary=19250

D190
Kim Arlich, id=E10001, salary=57000
Timothy Grove, id=E16398, salary=29900
```





### Map and reduce


### =ES5=


```javascript
var collectDept = function (arrOfObj) {
  var collect = arrOfObj.reduce(function (rtnObj, obj) {
    if (rtnObj[obj.dept] === undefined) {
      rtnObj[obj.dept] = [];
    }
    rtnObj[obj.dept].push(obj);
    return rtnObj;
  }, {});

  return Object.keys(collect).map(function (key) {
    return collect[key];
  });
};

var sortSalary = function (arrOfSalaryArrs) {
  return arrOfSalaryArrs.map(function (item) {
    return item.sort(function (a, b) {
      if (a.salary > b.salary) { return -1; }
      if (a.salary < b.salary) { return 1; }
      return 0;
    });
  });
};

var getNTopSalariesByDept = function (n, data) {
  if (n < 0) { return; }

  return sortSalary(collectDept(data)).map(function (list) {
    return list.slice(0,n);
  });
};
```


### =ES6=

By composition of generic functions:

```JavaScript
(() => {
    'use strict';

    // topNSalariesPerDept :: Int -> [[String]] -> [String]
    const topNSalariesPerDept = (n, records) =>
        foldl(
            (a, k, i) => (a[toLower(k)] = x => x[i], a),
            this,
            head(records)
        ) && map(intercalate(','),
            concatMap(take(n),
                reverse(
                    groupBy(
                        on(same, department),
                        sortBy(
                            flip(
                                mappendComparing([
                                    department,
                                    salary
                                ])
                            ),
                            tail(records)
                        )
                    )
                )
            )
        );

    // GENERIC FUNCTIONS -----------------------------------------------------

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs.map(f));
        })() : [];

    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat(Array.from(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const groupBy = (f, xs) => {
        const dct = xs.slice(1)
            .reduce((a, x) => {
                const
                    h = a.active.length > 0 ? a.active[0] : undefined,
                    blnGroup = h !== undefined && f(h, x);
                return {
                    active: blnGroup ? a.active.concat([x]) : [x],
                    sofar: blnGroup ? a.sofar : a.sofar.concat([a.active])
                };
            }, {
                active: xs.length > 0 ? [xs[0]] : [],
                sofar: []
            });
        return dct.sofar.concat(dct.active.length > 0 ? [dct.active] : []);
    };

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // intercalate :: String -> [a] -> String
    const intercalate = curry((s, xs) => xs.join(s));

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // mappendComparing :: [(a -> b)] -> (a -> a -> Ordering)
    const mappendComparing = fs => (x, y) =>
        fs.reduce((ord, f) => (ord !== 0) ? (
            ord
        ) : (() => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : a > b ? 1 : 0
        })(), 0);

    // on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    const on = (f, g) => (a, b) => f(g(a), g(b));

    // reverse :: [a] -> [a]
    const reverse = xs =>
        typeof xs === 'string' ? (
            xs.split('')
            .reverse()
            .join('')
        ) : xs.slice(0)
        .reverse();

    // same :: a -> a -> Bool
    const same = (a, b) => a === b

    // show :: Int -> a -> Indented String
    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[1], null, x[0]] : x
        );

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) =>
        xs.slice()
        .sort(f);

    // tail :: [a] -> [a]
    const tail = xs => xs.length ? xs.slice(1) : undefined;

    // take :: Int -> [a] -> [a]
    const take = curry((n, xs) => xs.slice(0, n));

    // toLower :: Text -> Text
    const toLower = s => s.toLowerCase();

    // TEST ------------------------------------------------------------------

    const xs = [
        ["Employee Name", "Employee ID", "Salary", "Department"],
        ["Tyler Bennett", "E10297", "32000", "D101"],
        ["John Rappl", "E21437", "47000", "D050"],
        ["George Woltman", "E00127", "53500", "D101"],
        ["Adam Smith", "E63535", "18000", "D202"],
        ["Claire Buckman", "E39876", "27800", "D202"],
        ["David McClellan", "E04242", "41500", "D101"],
        ["Rich Holcomb", "E01234", "49500", "D202"],
        ["Nathan Adams", "E41298", "21900", "D050"],
        ["Richard Potter", "E43128", "15900", "D101"],
        ["David Motsinger", "E27002", "19250", "D202"],
        ["Tim Sampair", "E03033", "27000", "D101"],
        ["Kim Arlich", "E10001", "57000", "D190"],
        ["Timothy Grove", "E16398", "29900", "D190"]
    ];

    return show(2,
        topNSalariesPerDept(3, xs)
    );
})();
```

```txt
[
  "John Rappl,E21437,47000,D050",
  "Nathan Adams,E41298,21900,D050",
  "George Woltman,E00127,53500,D101",
  "David McClellan,E04242,41500,D101",
  "Tyler Bennett,E10297,32000,D101",
  "Kim Arlich,E10001,57000,D190",
  "Timothy Grove,E16398,29900,D190",
  "Rich Holcomb,E01234,49500,D202",
  "Claire Buckman,E39876,27800,D202",
  "David Motsinger,E27002,19250,D202"
]
```



## jq

The task description invites use of the "language-native" data structure, which for jq is JSON, and so the following assumes that the file data.json contains an array of objects, each having as keys the strings on the header line.  Thus, the first object in the array looks like this:

```json
  {
    "Employee Name": "Tyler Bennett",
    "Employee ID": "E10297",
    "Salary": "32000",
    "Department": "D101"
  }
```


### Program


```jq
def top_rank_per_department(n):
  group_by(.Department)
  | reduce .[] as $dept
     ([]; ($dept
           | map(.Salary)
           | sort            # from least to most
           | .[length - n:]  # top n salaries
           | reverse) as $max
         | ($dept[0] | .Department) as $dept
         | . + [ { "Department": $dept, "top_salaries": $max } ] );

```



### Example

With the above program, the top two salaries in each dapartment can be found as shown in the following transcript:
```json

$ jq 'top_rank_per_department(2) data.json
[
  {
    "Department": "D050",
    "top_salaries": [
      "47000",
      "21900"
    ]
  },
  {
    "Department": "D101",
    "top_salaries": [
      "53500",
      "41500"
    ]
  },
  {
    "Department": "D190",
    "top_salaries": [
      "57000",
      "29900"
    ]
  },
  {
    "Department": "D202",
    "top_salaries": [
      "49500",
      "27800"
    ]
  }
]
```



## Jsish

Based on Javascript, imperative solution

```javascript
#!/usr/bin/env jsish
/* Top rank per group, in Jsish */
function top_rank(n) {
    var by_dept = group_by_dept(data);
    for (var dept in by_dept) {
        puts(dept);
        for (var i = 0; i < n && i < by_dept[dept].length; i++) {
            var emp = by_dept[dept][i];
            puts(emp.name + ", id=" + emp.id + ", salary=" + emp.salary);
        }
        puts("");
    }
}

// group by dept, and sort by salary
function group_by_dept(data) {
    var by_dept = {};
    for (var idx in data)  {
        var dept = data[idx].dept;
        if ( !by_dept.hasOwnProperty(dept)) {
            by_dept[dept] = new Array();
        }
        by_dept[dept].push(data[idx]);
    }
    for (var dept in by_dept) {
        by_dept[dept].sort(function (a,b) { return b.salary - a.salary; });
    }
    return by_dept;
}

if (Interp.conf('unitTest')) {
    var data = [
        {name: "Tyler Bennett",   id: "E10297", salary: 32000, dept: "D101"},
        {name: "John Rappl",      id: "E21437", salary: 47000, dept: "D050"},
        {name: "George Woltman",  id: "E00127", salary: 53500, dept: "D101"},
        {name: "Adam Smith",      id: "E63535", salary: 18000, dept: "D202"},
        {name: "Claire Buckman",  id: "E39876", salary: 27800, dept: "D202"},
        {name: "David McClellan", id: "E04242", salary: 41500, dept: "D101"},
        {name: "Rich Holcomb",    id: "E01234", salary: 49500, dept: "D202"},
        {name: "Nathan Adams",    id: "E41298", salary: 21900, dept: "D050"},
        {name: "Richard Potter",  id: "E43128", salary: 15900, dept: "D101"},
        {name: "David Motsinger", id: "E27002", salary: 19250, dept: "D202"},
        {name: "Tim Sampair",     id: "E03033", salary: 27000, dept: "D101"},
        {name: "Kim Arlich",      id: "E10001", salary: 57000, dept: "D190"},
        {name: "Timothy Grove",   id: "E16398", salary: 29900, dept: "D190"}
    ];

    top_rank(3);
}

/*
=!EXPECTSTART!=
D050
John Rappl, id=E21437, salary=47000
Nathan Adams, id=E41298, salary=21900

D101
George Woltman, id=E00127, salary=53500
David McClellan, id=E04242, salary=41500
Tyler Bennett, id=E10297, salary=32000

D190
Kim Arlich, id=E10001, salary=57000
Timothy Grove, id=E16398, salary=29900

D202
Rich Holcomb, id=E01234, salary=49500
Claire Buckman, id=E39876, salary=27800
David Motsinger, id=E27002, salary=19250

=!EXPECTEND!=
*/
```


```txt
prompt$ jsish --U topRankPerGroup.jsi
D050
John Rappl, id=E21437, salary=47000
Nathan Adams, id=E41298, salary=21900

D101
George Woltman, id=E00127, salary=53500
David McClellan, id=E04242, salary=41500
Tyler Bennett, id=E10297, salary=32000

D190
Kim Arlich, id=E10001, salary=57000
Timothy Grove, id=E16398, salary=29900

D202
Rich Holcomb, id=E01234, salary=49500
Claire Buckman, id=E39876, salary=27800
David Motsinger, id=E27002, salary=19250

prompt$ jsish -u topRankPerGroup.jsi
[PASS] topRankPerGroup.jsi
```



## Julia


```julia
# v0.6.0

using DataFrames

df = DataFrame(
EmployeeName=["Tyler Bennett", "John Rappl", "George Woltman", "Adam Smith",
"Claire Buckman", "David McClellan", "Rich Holcomb", "Nathan Adams",
"Richard Potter", "David Motsinger", "Tim Sampair", "Kim Arlich", "Timothy Grove"],
EmployeeID = ["E10297", "E21437", "E00127", "E63535", "E39876", "E04242",
"E01234", "E41298", "E43128", "E27002", "E03033", "E10001", "E16398"],
Salary = [32000, 47000, 53500, 18000, 27800, 41500, 49500, 21900, 15900, 19250,
27000, 57000, 29900],
Department = ["D101", "D050", "D101", "D202", "D202", "D101", "D202", "D050",
"D101", "D202", "D101", "D190", "D190"])

# To get only values
function firstnby(n::Int, y::Array, by::Array)
    # Check that each value belong to one and one only class
    if length(y) != length(by); error("y and by must have the same length"); end

    # Initialize resulting dictionary
    rst = Dict{eltype(by), Array{eltype(y)}}()

    # For each class...
    for cl in unique(by)
        # ...select the values of that class...
        i = find(x -> x == cl, by)
        # ...sort them and store them in result...
        rst[cl] = sort(y[i]; rev=true)
        # ...if length is greater than n select only first n elements
        if length(i) > n
            rst[cl] = rst[cl][1:n]
        end
    end
    return rst
end

for (cl, val) in firstnby(3, Array(df[:Salary]), Array(df[:Department]))
    println("$cl => $val")
end

# To get the full row...
function firstnby(n::Int, df::DataFrame, y::Symbol, by::Symbol)
    rst = Dict{eltype(df[by]), DataFrame}()

    for cl in unique(df[by])
        i = find(x -> x == cl, df[by])
        rst[cl] = sort(df[i, :]; cols=order(y; rev=true))
        if length(i) > n
            rst[cl] = rst[cl][1:n, :]
        end
    end
    return rst
end

for (cl, data) in firstnby(3, df, :Salary, :Department)
    println("\n$cl:\n$data")
end

```


```txt
D202 => [49500, 27800, 19250]
D101 => [53500, 41500, 32000]
D050 => [47000, 21900]
D190 => [57000, 29900]

D202:
3×4 DataFrames.DataFrame
│ Row │ EmployeeName      │ EmployeeID │ Salary │ Department │
├─────┼───────────────────┼────────────┼────────┼────────────┤
│ 1   │ "Rich Holcomb"    │ "E01234"   │ 49500  │ "D202"     │
│ 2   │ "Claire Buckman"  │ "E39876"   │ 27800  │ "D202"     │
│ 3   │ "David Motsinger" │ "E27002"   │ 19250  │ "D202"     │

D101:
3×4 DataFrames.DataFrame
│ Row │ EmployeeName      │ EmployeeID │ Salary │ Department │
├─────┼───────────────────┼────────────┼────────┼────────────┤
│ 1   │ "George Woltman"  │ "E00127"   │ 53500  │ "D101"     │
│ 2   │ "David McClellan" │ "E04242"   │ 41500  │ "D101"     │
│ 3   │ "Tyler Bennett"   │ "E10297"   │ 32000  │ "D101"     │

D050:
2×4 DataFrames.DataFrame
│ Row │ EmployeeName   │ EmployeeID │ Salary │ Department │
├─────┼────────────────┼────────────┼────────┼────────────┤
│ 1   │ "John Rappl"   │ "E21437"   │ 47000  │ "D050"     │
│ 2   │ "Nathan Adams" │ "E41298"   │ 21900  │ "D050"     │

D190:
2×4 DataFrames.DataFrame
│ Row │ EmployeeName    │ EmployeeID │ Salary │ Department │
├─────┼─────────────────┼────────────┼────────┼────────────┤
│ 1   │ "Kim Arlich"    │ "E10001"   │ 57000  │ "D190"     │
│ 2   │ "Timothy Grove" │ "E16398"   │ 29900  │ "D190"     │

```



## Kotlin


```scala
// version 1.1.2

data class Employee(val name: String, val id: String, val salary: Int, val dept: String)

const val N = 2 //say

fun main(args: Array<String>) {
    val employees = listOf(
        Employee("Tyler Bennett", "E10297", 32000, "D101"),
        Employee("John Rappl", "E21437", 47000, "D050"),
        Employee("George Woltman" , "E00127", 53500, "D101"),
        Employee("Adam Smith", "E63535", 18000, "D202"),
        Employee("Claire Buckman", "E39876", 27800, "D202"),
        Employee("David McClellan", "E04242", 41500, "D101"),
        Employee("Rich Holcomb", "E01234", 49500, "D202"),
        Employee("Nathan Adams", "E41298", 21900, "D050"),
        Employee("Richard Potter", "E43128", 15900, "D101"),
        Employee("David Motsinger", "E27002", 19250, "D202"),
        Employee("Tim Sampair", "E03033", 27000, "D101"),
        Employee("Kim Arlich", "E10001", 57000, "D190"),
        Employee("Timothy Grove", "E16398", 29900, "D190")
    )
    val employeesByDept = employees.sortedBy { it.dept }.groupBy { it.dept }
    println("Highest $N salaries by department:\n")
    for ((key, value) in employeesByDept) {
        val topRanked = value.sortedByDescending { it.salary }.take(N)
        println("Dept $key => ")
        for (i in 0 until N) with (topRanked[i]) { println("${name.padEnd(15)} $id $salary") }
        println()
    }
}
```


```txt

Highest 2 salaries by department:

Dept D050 =>
John Rappl      E21437 47000
Nathan Adams    E41298 21900

Dept D101 =>
George Woltman  E00127 53500
David McClellan E04242 41500

Dept D190 =>
Kim Arlich      E10001 57000
Timothy Grove   E16398 29900

Dept D202 =>
Rich Holcomb    E01234 49500
Claire Buckman  E39876 27800

```



## Lua


```lua
N = 2

lst = { { "Tyler Bennett","E10297",32000,"D101" },
	{ "John Rappl","E21437",47000,"D050" },
	{ "George Woltman","E00127",53500,"D101" },
	{ "Adam Smith","E63535",18000,"D202" },
	{ "Claire Buckman","E39876",27800,"D202" },
	{ "David McClellan","E04242",41500,"D101" },
	{ "Rich Holcomb","E01234",49500,"D202" },
	{ "Nathan Adams","E41298",21900,"D050" },
	{ "Richard Potter","E43128",15900,"D101" },
	{ "David Motsinger","E27002",19250,"D202" },
	{ "Tim Sampair","E03033",27000,"D101" },
	{ "Kim Arlich","E10001",57000,"D190" },
	{ "Timothy Grove","E16398",29900,"D190" }
      }

dep = {}
for i = 1, #lst do
    if dep[ lst[i][4] ] == nil then
	dep[ lst[i][4] ] = {}
	dep[ lst[i][4] ][1] = lst[i]
    else
	dep[ lst[i][4] ][#dep[lst[i][4]]+1] = lst[i]
    end
end

for i, _ in pairs( dep ) do
    table.sort( dep[i], function (a,b) return a[3] > b[3] end )

    print( "Department:", dep[i][1][4] )
    for l = 1, math.min( N, #dep[i] ) do
	print( "", dep[i][l][1], dep[i][l][2], dep[i][l][3] )
    end
    print ""
end
```
<pre style="height:30ex;overflow:scroll">Department:	D050
	John Rappl	E21437	47000
	Nathan Adams	E41298	21900

Department:	D202
	Rich Holcomb	E01234	49500
	Claire Buckman	E39876	27800

Department:	D190
	Kim Arlich	E10001	57000
	Timothy Grove	E16398	29900

Department:	D101
	George Woltman	E00127	53500
	David McClellan	E04242	41500
```


## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      ' erase stack of values, so we can add data
      Flush
      Input "N=",N
      Enum Departments {D050,D101,D190,D202}

      \\ Inventory Department need unique keys
      Inventory Department
      \\ each item in this inventory should be an inventory too
      Class Empl {
            name$, id$, salary
            Class:
            Module Empl(.name$, .id$, .salary) {}
      }
      Data "Tyler Bennett","E10297",32000,D101
      Data "John Rappl","E21437",47000,D050
      Data "George Woltman","E00127",53500,D101
      Data "Adam Smith","E63535",18000,D202
      Data "Claire Buckman","E39876",27800,D202
      Data "David McClellan","E04242",41500,D101
      Data "Rich Holcomb","E01234",49500,D202
      Data "Nathan Adams","E41298",21900,D050
      Data "Richard Potter","E43128",15900,D101
      Data "David Motsinger","E27002",19250,D202
      Data "Tim Sampair","E03033",27000,D101
      Data "Kim Arlich","E10001",57000,D190
      Data "Timothy Grove","E16398",29900,D190
      Data ""
      Read name$
      While name$<>""  {
            Read id$, salary, dep
            Rem : Print name$, id$, salary, dep
            If Exist(Department, dep) Then {
                  z=Eval(Department)    ' get pointer to inventory
                  AppendOne()
            } Else {
                  z=queue
                  AppendDep()
                  AppendOne()
            }
            Read name$
      }
      Sort Department as number
      i=each(Department)
      \\ make depname as type of Departments
      depname=D050
      Print  "Dep.  Employee Name       Emp. ID Salary"
      While i {
            \\ when we pass a number to a enum variable
            \\ if the number exist, get that enum item else raise error
            depname=val(eval$(i, i^))
            \\ z is a pointer to inventory
            z=Eval(i)
            Sort descending z as number
            k=each(z,1,N)
            While k {
                  Empl=Eval(k)
                  For Empl {
                        \\ eval$(depname) return the name of enum variable (like D050)
                        Print Format$("{0:6}{1:20}{2:8}{3::-8}",Eval$(depname), .name$, .id$, .salary)
                  }
            }
      }
      Print "Done"
      Sub AppendDep()
            Append Department, dep:=z
      End Sub
      Sub AppendOne()
                  Append z, salary:=Empl(name$, id$, salary)
      End Sub
}
Checkit

```

<pre style="height:30ex;overflow:scroll">
N=1
Dep.  Employee Name       Emp. ID Salary
D050  John Rappl          E21437     47000
D101  George Woltman      E00127     53500
D190  Kim Arlich          E10001     57000
D202  Rich Holcomb        E01234     49500
Done

N=2
Dep.  Employee Name       Emp. ID Salary
D050  John Rappl          E21437     47000
D050  Nathan Adams        E41298     21900
D101  George Woltman      E00127     53500
D101  David McClellan     E04242     41500
D190  Kim Arlich          E10001     57000
D190  Timothy Grove       E16398     29900
D202  Rich Holcomb        E01234     49500
D202  Claire Buckman      E39876     27800
Done
</pre >


## Mathematica


```Mathematica
InitialList ={{"Tyler Bennett","E10297",32000,"D101"},
{"John Rappl","E21437",47000,"D050"},{"George Woltman","E00127",53500,"D101"},
{"Adam Smith","E63535",18000,"D202"},{"Claire Buckman","E39876",27800,"D202"},
{"David McClellan","E04242",41500,"D101"},{"Rich Holcomb","E01234",49500,"D202"},
{"Nathan Adams","E41298",21900,"D050"},{"Richard Potter","E43128",15900,"D101"},
{"David Motsinger","E27002",19250,"D202"},{"Tim Sampair","E03033",27000,"D101"},
{"Kim Arlich","E10001",57000,"D190"},{"Timothy Grove","E16398",29900,"D190"}};

TrimmedList=Map[ If[Length[#]>3,Take[#,3],#]& ,
 Map[Reverse[SortBy[#,#[[3]]&]]&,GatherBy[InitialList,Last]]
];

Scan[((Print["Department ",#[[1,4]],"\n","Employee","\t","Id","\t","Salary"]&[#])&[#];
(Scan[Print[#[[1]],"\t",#[[2]],"\t",#[[3]]]&,#] )& [#])&,TrimmedList]
```
<pre style="height:30ex;overflow:scroll">
```txt
Department D101
Employee	Id	Salary
George Woltman	E00127	53500
David McClellan	E04242	41500
Tyler Bennett	E10297	32000
Department D050
Employee	Id	Salary
John Rappl	E21437	47000
Nathan Adams	E41298	21900
Department D202
Employee	Id	Salary
Rich Holcomb	E01234	49500
Claire Buckman	E39876	27800
David Motsinger	E27002	19250
Department D190
Employee	Id	Salary
Kim Arlich	E10001	57000
Timothy Grove	E16398	29900
```



## Nim

```nim
import algorithm

type Record = tuple[name, id: string, salary: int, department: string]

var people: seq[Record] =
  @[("Tyler Bennett", "E10297", 32000, "D101"),
    ("John Rappl", "E21437", 47000, "D050"),
    ("George Woltman", "E00127", 53500, "D101"),
    ("Adam Smith", "E63535", 18000, "D202"),
    ("Claire Buckman", "E39876", 27800, "D202"),
    ("David McClellan", "E04242", 41500, "D101"),
    ("Rich Holcomb", "E01234", 49500, "D202"),
    ("Nathan Adams", "E41298", 21900, "D050"),
    ("Richard Potter", "E43128", 15900, "D101"),
    ("David Motsinger", "E27002", 19250, "D202"),
    ("Tim Sampair", "E03033", 27000, "D101"),
    ("Kim Arlich", "E10001", 57000, "D190"),
    ("Timothy Grove", "E16398", 29900, "D190")]

proc pcmp(a, b): int =
  result = cmp(a.department, b.department)
  if result != 0: return
  result = cmp(b.salary, a.salary)

proc top(n) =
  sort(people, pcmp)

  var rank = 0
  for i, p in people:
    if i > 0 and p.department != people[i-1].department:
      rank = 0
      echo ""

    if rank < n:
      echo p.department," ",p.salary," ",p.name

    inc rank

top(2)
```

Output:

```txt
D050 47000 John Rappl
D050 21900 Nathan Adams

D101 53500 George Woltman
D101 41500 David McClellan

D190 57000 Kim Arlich
D190 29900 Timothy Grove

D202 49500 Rich Holcomb
D202 27800 Claire Buckman
```



## OCaml


```ocaml
open StdLabels

let to_string (name,_,s,_) = (Printf.sprintf "%s (%d)" name s)

let take n li =
  let rec aux i acc = function
  | _ when i >= n -> (List.rev acc)
  | [] -> (List.rev acc)
  | x::xs -> aux (succ i) (x::acc) xs
  in
  aux 0 [] li ;;

let toprank data n =
  let len = List.length data in
  let h = Hashtbl.create len in
  List.iter data ~f:(fun ((_,_,_,dep) as employee) ->
    Hashtbl.add h dep employee);
  let deps =
    List.fold_left data ~init:[] ~f:
      (fun ac (_,_,_,v) -> if List.mem v ac then ac else v::ac) in
  let f dep =
    Printf.printf "Department: %s\n " dep;
    let l = Hashtbl.find_all h dep in
    let l2 = List.sort (fun (_,_,v1,_) (_,_,v2,_) -> compare v2 v1) l in
    let l3 = (take n l2) in
    print_endline(String.concat ", " (List.map to_string l3));
    print_newline()
  in
  List.iter f deps;
;;

let data = [
  "Tyler Bennett",   "E10297",  32000,  "D101";
  "John Rappl",      "E21437",  47000,  "D050";
  "George Woltman",  "E00127",  53500,  "D101";
  "Adam Smith",      "E63535",  18000,  "D202";
  "Claire Buckman",  "E39876",  27800,  "D202";
  "David McClellan", "E04242",  41500,  "D101";
  "Rich Holcomb",    "E01234",  49500,  "D202";
  "Nathan Adams",    "E41298",  21900,  "D050";
  "Richard Potter",  "E43128",  15900,  "D101";
  "David Motsinger", "E27002",  19250,  "D202";
  "Tim Sampair",     "E03033",  27000,  "D101";
  "Kim Arlich",      "E10001",  57000,  "D190";
  "Timothy Grove",   "E16398",  29900,  "D190";
]

let () =
  toprank data 3;
;;
```
<pre style="height:30ex;overflow:scroll">Department: D190
 Kim Arlich (57000), Timothy Grove (29900)

Department: D202
 Rich Holcomb (49500), Claire Buckman (27800), David Motsinger (19250)

Department: D050
 John Rappl (47000), Nathan Adams (21900)

Department: D101
 George Woltman (53500), David McClellan (41500), Tyler Bennett (32000)

```

--[[User:Bbsingapore|Bbsingapore]] 10:00, 13 January 2012 (UTC)


## Oforth


First sort by department. Then group elements with same department.

Then sort each department by salaries and keep only the first n elements.


```Oforth
Object Class new: Employee(name, id, salary, dep)

Employee method: initialize  := dep  := salary := id := name ;
Employee method: salary  @salary ;
Employee method: dep     @dep ;
Employee method: <<      "[" << @dep << "," << @name << "," << @salary << "]" << ;

: topRank(n)
| employees |
   ListBuffer new ->employees

   Employee new("Tyler Bennett",   "E10297", 32000, "D101") employees add
   Employee new("John Rappl",      "E21437", 47000, "D050") employees add
   Employee new("George Woltman",  "E00127", 53500, "D101") employees add
   Employee new("Adam Smith",      "E63535", 18000, "D202") employees add
   Employee new("Claire Buckman",  "E39876", 27800, "D202") employees add
   Employee new("David McClellan", "E04242", 41500, "D101") employees add
   Employee new("Rich Holcomb",    "E01234", 49500, "D202") employees add
   Employee new("Nathan Adams",    "E41298", 21900, "D050") employees add
   Employee new("Richard Potter",  "E43128", 15900, "D101") employees add
   Employee new("David Motsinger", "E27002", 19250, "D202") employees add
   Employee new("Tim Sampair",     "E03033", 27000, "D101") employees add
   Employee new("Kim Arlich",      "E10001", 57000, "D190") employees add
   Employee new("Timothy Grove",   "E16398", 29900, "D190") employees add

   #dep employees sortBy groupWith( #dep )
   map(#[ sortBy(#[ salary neg ]) left(n) ]) apply(#println) ;
```


```txt

>topRank(3)
[[D050,John Rappl,47000], [D050,Nathan Adams,21900]]
[[D101,George Woltman,53500], [D101,David McClellan,41500], [D101,Tyler Bennett,32000]]
[[D190,Kim Arlich,57000], [D190,Timothy Grove,29900]]
[[D202,Rich Holcomb,49500], [D202,Claire Buckman,27800], [D202,David Motsinger,19250]]

```



## Oz


```oz
declare
  %% Create a list of employee records.
  Data = {Map
          [['Tyler Bennett' e10297 32000 d101]
           ['John Rappl' e21437 47000 d050]
           ['George Woltman' e00127 53500 d101]
           ['Adam Smith' e63535 18000 d202]
           ['Claire Buckman' e39876 27800 d202]
           ['David McClellan' e04242 41500 d101]
           ['Rich Holcomb' e01234 49500 d202]
           ['Nathan Adams' e41298 21900 d050]
           ['Richard Potter' e43128 15900 d101]
           ['David Motsinger' e27002 19250 d202]
           ['Tim Sampair' e03033 27000 d101]
           ['Kim Arlich' e10001 57000 d190]
           ['Timothy Grove' e16398 29900 d190]]

          fun {$ [Name Id Salary Department]}
             employee(name:Name id:Id salary:Salary department:Department)
          end}

  fun {TopEarners Employees N}
     {Record.map {GroupBy Employees department}
      fun {$ Employees}
	 {List.take
	  {Sort Employees CompareSalary}
	  N}
      end}
  end

  fun {CompareSalary E1 E2}
     E1.salary > E2.salary
  end

  %% Groups the records Xs by the value of feature F and returns
  %% the result as a record that maps values of F to list of records.
  fun {GroupBy Xs F}
     Groups = {Dictionary.new}
  in
     for X in Xs do
        Groups.(X.F) := X|{CondSelect Groups X.F nil}
     end
     {Dictionary.toRecord unit Groups}
  end
in
  {Inspect {TopEarners Data 3}}
```



## PARI/GP

```parigp
{V=[["Tyler Bennett","E10297",32000,"D101"],
["John Rappl","E21437",47000,"D050"],
["George Woltman","E00127",53500,"D101"],
["Adam Smith","E63535",18000,"D202"],
["Claire Buckman","E39876",27800,"D202"],
["David McClellan","E04242",41500,"D101"],
["Rich Holcomb","E01234",49500,"D202"],
["Nathan Adams","E41298",21900,"D050"],
["Richard Potter","E43128",15900,"D101"],
["David Motsinger","E27002",19250,"D202"],
["Tim Sampair","E03033",27000,"D101"],
["Kim Arlich","E10001",57000,"D190"],
["Timothy Grove","E16398",29900,"D190"]]};

top(n,V)={
  my(dept=vecsort(vector(#V,i,V[i][4]),,8),d,v);
  for(i=1,#dept,
    d=dept[i];
    print(d);
    v=select(u->u[4]==d,V);
    v=vecsort(v,3,4); \\ Sort by salary (#3) descending (flag 0x4)
    for(j=1,min(n,#v),
      print("\t",v[j][1],"\t",v[j][2],"\t",v[j][3])
    )
  );
};

top(2,V)
```



## Pascal

```pascal
program TopRankPerGroup(output);

uses
  Classes, Math;

type
  TData = record
            name:   string;
	    ID:     string;
	    salary: longint;
	    dept:   string
	  end;
  PTData = ^TData;

const
  data: array [1..13] of TData =
    ( (name: 'Tyler Bennett';   ID: 'E10297'; salary: 32000; dept: 'D101'),
      (name: 'John Rappl';      ID: 'E21437'; salary: 47000; dept: 'D050'),
      (name: 'George Woltman';  ID: 'E00127'; salary: 53500; dept: 'D101'),
      (name: 'Adam Smith';      ID: 'E63535'; salary: 18000; dept: 'D202'),
      (name: 'Claire Buckman';  ID: 'E39876'; salary: 27800; dept: 'D202'),
      (name: 'David McClellan'; ID: 'E04242'; salary: 41500; dept: 'D101'),
      (name: 'Rich Holcomb';    ID: 'E01234'; salary: 49500; dept: 'D202'),
      (name: 'Nathan Adams';    ID: 'E41298'; salary: 21900; dept: 'D050'),
      (name: 'Richard Potter';  ID: 'E43128'; salary: 15900; dept: 'D101'),
      (name: 'David Motsinger'; ID: 'E27002'; salary: 19250; dept: 'D202'),
      (name: 'Tim Sampair';     ID: 'E03033'; salary: 27000; dept: 'D101'),
      (name: 'Kim Arlich';      ID: 'E10001'; salary: 57000; dept: 'D190'),
      (name: 'Timothy Grove';   ID: 'E16398'; salary: 29900; dept: 'D190')
    );

function CompareSalary(Item1, Item2: PTData): longint;
  begin
    CompareSalary := Item2^.salary - Item1^.salary;
  end;

var
  depts   : TStringList;
  deptList: Tlist;
  number, i, j: integer;

begin
  write ('Enter the number of ranks: ');
  readln (number);
  depts := TStringList.Create;
  depts.Sorted := true;
  depts.Duplicates := dupIgnore;
  for i := low(data) to high(data) do
    depts.Add(data[i].dept);

  for i := 0 to depts.Count - 1 do
  begin
    writeln;
    writeln('Department: ', depts.Strings[i]);
    deptList := TList.Create;
    for j := low(data) to high(data) do
      if data[j].dept = depts.Strings[i] then
        deptList.Add(@data[j]);
    deptList.Sort(TListSortCompare(@CompareSalary));
    for j := 0 to min(deptList.count, number) - 1 do
    begin
      write (PTData(deptList.Items[j])^.name, ', ');
      write ('ID: ', PTData(deptList.Items[j])^.ID, ', ');
      write ('Salary: ', PTData(deptList.Items[j])^.Salary);
      writeln;
    end;
    deptList.Destroy;
  end;
end.

```
<pre style="height:30ex;overflow:scroll">% ./TopRankPerGroup
Enter the number of ranks: 3

Department: D050
John Rappl, ID: E21437, Salary: 47000
Nathan Adams, ID: E41298, Salary: 21900

Department: D101
George Woltman, ID: E00127, Salary: 53500
David McClellan, ID: E04242, Salary: 41500
Tyler Bennett, ID: E10297, Salary: 32000

Department: D190
Kim Arlich, ID: E10001, Salary: 57000
Timothy Grove, ID: E16398, Salary: 29900

Department: D202
Rich Holcomb, ID: E01234, Salary: 49500
Claire Buckman, ID: E39876, Salary: 27800
David Motsinger, ID: E27002, Salary: 19250
```



## Perl


```perl
sub zip {
    my @a = @{shift()};
    my @b = @{shift()};
    my @l;
    push @l, shift @a, shift @b while @a and @b;
    return @l;
}

sub uniq {
    my %h;
    grep {!$h{$_}++} @_;
}

my @data =
    map {{ zip [qw(name id salary dept)], [split ','] }}
    split "\n",
    <<'EOF';
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190
EOF

my $N = shift || 3;

foreach my $d (uniq sort map {$_->{dept}} @data) {
    print "$d\n";
    my @es =
        sort {$b->{salary} <=> $a->{salary}}
        grep {$_->{dept} eq $d}
        @data;
    foreach (1 .. $N) {
        @es or last;
        my $e = shift @es;
        printf "%-15s | %-6s | %5d\n", @{$e}{qw(name id salary)};
    }
    print "\n";
}
```

```txt
D050
John Rappl      | E21437 | 47000
Nathan Adams    | E41298 | 21900

D101
George Woltman  | E00127 | 53500
David McClellan | E04242 | 41500
Tyler Bennett   | E10297 | 32000

D190
Kim Arlich      | E10001 | 57000
Timothy Grove   | E16398 | 29900

D202
Rich Holcomb    | E01234 | 49500
Claire Buckman  | E39876 | 27800
David Motsinger | E27002 | 19250
```



## Perl 6

We use whitespace-separated fields here from a heredoc; <tt>q:to/---/</tt> begins the heredoc. The <tt>Z=></tt> operator zips two lists into a list of pairs.
In <tt>MAIN</tt>, the <tt>classify</tt> method generates pairs where each key is a different department, and each value all the entries in that department. We then sort the pairs and process each department separately.  Within each department, we sort on salary (negated to reverse the order).  The last statement is essentially a list comprehension that uses a slice subscript with the <tt>^</tt> "up to" operator to take the first N elements of the sorted employee list. The <tt>:v</tt> modifier returns only valid values. The <tt>.<Name></tt> form is a slice hash subscript with literals strings.  That in turn is just the subscript form of the <tt><...></tt> ("quote words") form, which is more familar to Perl 5 programmers as
<tt>qw/.../</tt>.  We used that form earlier to label the initial data set.

This program also makes heavy use of method calls that start with dot.  In Perl 6 this means a method call on the current topic, <tt>$_</tt>, which is automatically set by any <tt>for</tt> or <tt>map</tt> construct that doesn't declare an explicit formal parameter on its closure.


```perl6
my @data = do for q:to/---/.lines -> $line {
        E10297  32000   D101    Tyler Bennett
        E21437  47000   D050    John Rappl
        E00127  53500   D101    George Woltman
        E63535  18000   D202    Adam Smith
        E39876  27800   D202    Claire Buckman
        E04242  41500   D101    David McClellan
        E01234  49500   D202    Rich Holcomb
        E41298  21900   D050    Nathan Adams
        E43128  15900   D101    Richard Potter
        E27002  19250   D202    David Motsinger
        E03033  27000   D101    Tim Sampair
        E10001  57000   D190    Kim Arlich
        E16398  29900   D190    Timothy Grove
        ---

  $%( < Id      Salary  Dept    Name >
      Z=>
      $line.split(/ \s\s+ /)
    )
}

sub MAIN(Int $N = 3) {
    for @data.classify({ .<Dept> }).sort».value {
        my @es = .sort: { -.<Salary> }
        say '' if (state $bline)++;
        say .< Dept Id Salary Name > for @es[^$N]:v;
    }
}
```
<pre style="height:30ex;overflow:scroll">
D050 E21437 47000 John Rappl
D050 E41298 21900 Nathan Adams

D101 E00127 53500 George Woltman
D101 E04242 41500 David McClellan
D101 E10297 32000 Tyler Bennett

D190 E10001 57000 Kim Arlich
D190 E16398 29900 Timothy Grove

D202 E01234 49500 Rich Holcomb
D202 E39876 27800 Claire Buckman
D202 E27002 19250 David Motsinger
```



## Phix


```Phix
constant N=3

--                      Employee Name,Employee ID,Salary,Department
enum                    /*NAME,*/        /*ID,*/  SAL=3, DEPT=4
constant employees = {{"Tyler Bennett",  "E10297",32000,"D101"},
                      {"John Rappl",     "E21437",47000,"D050"},
                      {"George Woltman", "E00127",53500,"D101"},
                      {"Adam Smith",     "E63535",18000,"D202"},
                      {"Claire Buckman", "E39876",27800,"D202"},
                      {"David McClellan","E04242",41500,"D101"},
                      {"Rich Holcomb",   "E01234",49500,"D202"},
                      {"Nathan Adams",   "E41298",21900,"D050"},
                      {"Richard Potter", "E43128",15900,"D101"},
                      {"David Motsinger","E27002",19250,"D202"},
                      {"Tim Sampair",    "E03033",27000,"D101"},
                      {"Kim Arlich",     "E10001",57000,"D190"},
                      {"Timothy Grove",  "E16398",29900,"D190"}}

function by_dept_sal(integer i, integer j)
    return compare(employees[i][DEPT]&-employees[i][SAL],
                   employees[j][DEPT]&-employees[j][SAL])
end function

sequence tags = custom_sort(routine_id("by_dept_sal"),tagset(length(employees)))

string lastdep = ""
integer dcount = 0
printf(1,"Top %d salaries by department\n",{N})
for i=1 to length(employees) do
    object emp = employees[tags[i]]
    if emp[DEPT]!=lastdep then
        lastdep = emp[DEPT]
        dcount = 1
        printf(1,"\n")
    else
        dcount += 1
    end if
    if dcount<=N then
        ?emp
    end if
end for
```

```txt

Top 3 salaries by department

{"John Rappl","E21437",47000,"D050"}
{"Nathan Adams","E41298",21900,"D050"}

{"George Woltman","E00127",53500,"D101"}
{"David McClellan","E04242",41500,"D101"}
{"Tyler Bennett","E10297",32000,"D101"}

{"Kim Arlich","E10001",57000,"D190"}
{"Timothy Grove","E16398",29900,"D190"}

{"Rich Holcomb","E01234",49500,"D202"}
{"Claire Buckman","E39876",27800,"D202"}
{"David Motsinger","E27002",19250,"D202"}

```



## PHP


```php
$data = Array(
			Array("Tyler Bennett","E10297",32000,"D101"),
			Array("John Rappl","E21437",47000,"D050"),
			Array("George Woltman","E00127",53500,"D101"),
			Array("Adam Smith","E63535",18000,"D202"),
			Array("Claire Buckman","E39876",27800,"D202"),
			Array("David McClellan","E04242",41500,"D101"),
			Array("Rich Holcomb","E01234",49500,"D202"),
			Array("Nathan Adams","E41298",21900,"D050"),
			Array("Richard Potter","E43128",15900,"D101"),
			Array("David Motsinger","E27002",19250,"D202"),
			Array("Tim Sampair","E03033",27000,"D101"),
			Array("Kim Arlich","E10001",57000,"D190"),
			Array("Timothy Grove","E16398",29900,"D190")
			);
function top_sal($num){
	global $data;
	$depts = Array();
	foreach($data as $key => $arr){
		if(!isset($depts[$arr[3]])) $depts[$arr[3]] = Array();
		$depts[$arr[3]][] = $key;
	}
	function topsalsort($a,$b){
		global $data;
		if ($data[$a][2] == $data[$b][2]) {
			return 0;
		}
		return ($data[$a][2] < $data[$b][2]) ? 1 : -1;
	}
	foreach ($depts as $key => $val){
		usort($depts[$key],"topsalsort");
	}
	ksort($depts);
	echo '
```txt
';
	foreach ($depts as $key => $val){
		echo $key . '
';
		echo 'Name			ID		Salary
';
		$count = 0;
		foreach($val as $value){
			echo $data[$value][0] . '	' . $data[$value][1] . '	' . $data[$value][2] . '
';
			$count++;
			if($count>=$num) break;
		}
		echo '
';
	}
	echo '
```
';
}
top_sal(3);
```
<pre style="height:30ex;overflow:scroll">D050
Name			ID		Salary
John Rappl		E21437		47000
Nathan Adams		E41298		21900

D101
Name			ID		Salary
George Woltman		E00127		53500
David McClellan		E04242		41500
Tyler Bennett		E10297		32000

D190
Name			ID		Salary
Kim Arlich		E10001		57000
Timothy Grove		E16398		29900

D202
Name			ID		Salary
Rich Holcomb		E01234		49500
Claire Buckman		E39876		27800
David Motsinger		E27002		19250

```



## PicoLisp


```PicoLisp
# Employee Name, ID, Salary, Department
(de *Employees
   ("Tyler Bennett" E10297 32000 D101)
   ("John Rappl" E21437 47000 D050)
   ("George Woltman" E00127 53500 D101)
   ("Adam Smith" E63535 18000 D202)
   ("Claire Buckman" E39876 27800 D202)
   ("David McClellan" E04242 41500 D101)
   ("Rich Holcomb" E01234 49500 D202)
   ("Nathan Adams" E41298 21900 D050)
   ("Richard Potter" E43128 15900 D101)
   ("David Motsinger" E27002 19250 D202)
   ("Tim Sampair" E03033 27000 D101)
   ("Kim Arlich" E10001 57000 D190)
   ("Timothy Grove" E16398 29900 D190) )

(de topEmployees (N)
   (let Fmt (4 -16 -7 7)
      (for Dept (by cadddr group *Employees)
         (prinl "Department " (cadddr (car Dept)) ":")
         (tab Fmt NIL "Name" "ID" "Salary")
         (for (I . D) (flip (by caddr sort Dept))
            (tab Fmt (pack I ". ") (car D) (cadr D) (caddr D))
            (T (= I N)) )
         (prinl) ) ) )

(topEmployees 3)
```
<pre style="height:30ex;overflow:scroll">Department D101:
    Name            ID      Salary
 1. George Woltman  E00127   53500
 2. David McClellan E04242   41500
 3. Tyler Bennett   E10297   32000

Department D050:
    Name            ID      Salary
 1. John Rappl      E21437   47000
 2. Nathan Adams    E41298   21900

Department D202:
    Name            ID      Salary
 1. Rich Holcomb    E01234   49500
 2. Claire Buckman  E39876   27800
 3. David Motsinger E27002   19250

Department D190:
    Name            ID      Salary
 1. Kim Arlich      E10001   57000
 2. Timothy Grove   E16398   29900
```



## PL/I


```PL/I
(subrg, stringrange, stringsize):
rank: procedure options (main);   /* 10 November 2013 */

   declare 1 employee (13),
             2 name char (15) varying,
             2 ID   char (6),
             2 salary fixed (5),
             2 department char (4);
   declare done(hbound(employee)) bit (1);
   declare ptr(hbound(employee)) fixed binary;
   declare true bit(1) value ('1'b), false bit(1) value ('0'b);
   declare dept character (4);
   declare text character (80) varying;
   declare (i, j, l, k, m, n, p, t) fixed binary;
   declare in file input;

   open file (in) title ('/TOP-RANK.DAT, RECSIZE(80), TYPE(TEXT)' );

   on endfile (in) go to completed_input;
   j = 0;
   do forever;
      get file (in) edit (text) (L);
      j = j + 1;
      i = index(text, ',');
      name(j) = substr(text, 1, i-1);
      k = index(text, ',', i+1);
      ID(j) = substr(text, i+1, k-(i+1));
      i = k; k = index(text, ',', i+1);
      salary(j) = substr(text, i+1, k-(i+1));
      department(j) = substr(text, k+1);
   end;

completed_input:
   m = hbound(employee);
   put skip list ('How many highest-paid employees do you want?');
   get (n);
   put skip edit ('Looking for the ', trim(n),
                  ' highest-paid employees in each department') (a);
   done = false;
   do i = 1 to m;
      do j = 1 to m;
         if done(j) then iterate;
         dept = department(j);
          /* done(j) = true; */
         leave;
      end;
      /* Locate all the employees of this department. */
      k = 0;
      do j = 1 to m;
         if ^done(j) & (department(j) = dept) then
            do;
               k = k + 1;
               ptr(k) = j;
               done(j) = true;
            end;
      end;
      if k = 0 then leave; /* (No more departments.) */

      put skip list ('Employees in department ' || dept || ' are:-' );
      do j = 1 to k;
         put skip list (employee(ptr(j)));
      end;
      /* We now have k employees in "dept".  Now find the maximum n salaries. */
      /* ptr points to all of them. */
      /* Use a bubble sort to move n values to one end. */
      do p = 1 to min(n, k);
         do j = 1 to k-1;
            if salary(ptr(j)) > salary(ptr(j+1)) then
               do;
                  t = ptr(j+1); ptr(j+1) = ptr(j); ptr(j) = t;
               end;
         end;
      end;

      /* Having moved the largest n values to the end of our list, */
      /* we print them. */
      put skip list ('Highest-paid employees in department ' || dept || ':-');
      do j = k to k-min(k,n)+1 by -1;
         put skip list (employee(ptr(j)) );
      end;
   end;
   put skip list ('FINISHED');
end rank;
```
<pre style="height:30ex;overflow:scroll">How many highest-paid employees do you want?

Looking for the 2 highest-paid employees in each department
Employees in department D101 are:-
Tyler Bennett           E10297                     32000                D101
George Woltman          E00127                     53500                D101
David McClellan         E04242                     41500                D101
Richard Potter          E43128                     15900                D101
Tim Sampair             E03033                     27000                D101
Highest-paid employees in department D101:-
George Woltman          E00127                     53500                D101
David McClellan         E04242                     41500                D101
Employees in department D050 are:-
John Rappl              E21437                     47000                D050
Nathan Adams            E41298                     21900                D050
Highest-paid employees in department D050:-
John Rappl              E21437                     47000                D050
Nathan Adams            E41298                     21900                D050
Employees in department D202 are:-
Adam Smith              E63535                     18000                D202
Claire Buckman          E39876                     27800                D202
Rich Holcomb            E01234                     49500                D202
David Motsinger         E27002                     19250                D202
Highest-paid employees in department D202:-
Rich Holcomb            E01234                     49500                D202
Claire Buckman          E39876                     27800                D202
Employees in department D190 are:-
Kim Arlich              E10001                     57000                D190
Timothy Grove           E16398                     29900                D190
Highest-paid employees in department D190:-
Kim Arlich              E10001                     57000                D190
Timothy Grove           E16398                     29900                D190
FINISHED
```



## PL/SQL


```plsql
create or replace procedure "Top rank per group"(TOP_N in pls_integer default 3) as
  cursor CSR_EMP(TOP_N pls_integer) is
    select case LINE
             when 10 then
              'Tot.' || LPAD(POPULATION, 2) || ' Employees in ' || TIE_COUNT ||
              ' deps.Avg salary:' || TO_CHAR(SALARY, '99990.99')
             when 30 then
              '-'
             when 50 then
              'Department: ' || DEPT_ID || ', pop: ' || POPULATION ||
              '. Avg Salary: ' || TO_CHAR(SALARY, '99990.99')
             when 70 then
              LPAD('Employee ID', 14) || LPAD('Employee name', 20) ||
              LPAD('Salary', 9) || 'Rank'
             when 90 then
              LPAD('+', 14, '-') || LPAD('+', 20, '-') || LPAD('+', 9, '-') ||
              LPAD('+', 4, '-')
             else
              LPAD(' ', 8) || LPAD(EMP_ID, 6) || LPAD(EMP_NAME, 20) ||
              TO_CHAR(SALARY, '99990.99') ||
              LPAD(case when TIE_COUNT = 1 then  ' ' else 'T' end || RANK, 4)
           end "Top rank per group"
      from (select 10 LINE
                  ,null EMP_ID
                  ,null EMP_NAME
                  ,' ' DEPT_ID
                  ,avg(SALARY) SALARY
                  ,0 RANK
                  ,count(distinct DEPT_ID) TIE_COUNT
                  ,count(*) POPULATION
              from EMP
            union all
            select 30      LINE
                  ,null    EMP_ID
                  ,null    EMP_NAME
                  ,DEPT_ID
                  ,0       SALARY
                  ,0       RANK
                  ,0       TIE_COUNT
                  ,0       POPULATION
              from EMP
             group by DEPT_ID
            union all
            select 50 LINE
                  ,null EMP_ID
                  ,null EMP_NAME
                  ,DEPT_ID
                  ,avg(SALARY) SALARY
                  ,0 RANK
                  ,0 TIE_COUNT
                  ,count(*) POPULATION
              from EMP
             group by DEPT_ID
            union all
            select 70      LINE
                  ,null    EMP_ID
                  ,null    EMP_NAME
                  ,DEPT_ID
                  ,0       SALARY
                  ,0       RANK
                  ,0       TIE_COUNT
                  ,0       POPULATION
              from EMP
             group by DEPT_ID
            union all
            select 90      LINE
                  ,null    EMP_ID
                  ,null    EMP_NAME
                  ,DEPT_ID
                  ,0       SALARY
                  ,0       RANK
                  ,0       TIE_COUNT
                  ,0       POPULATION
              from EMP
             group by DEPT_ID
            union all
            select 110 LINE
                  ,EMP_ID
                  ,EMP_NAME
                  ,DEPT_ID
                  ,SALARY
                  ,(select count(distinct EMP4.SALARY)
                      from EMP EMP4
                     where EMP4.DEPT_ID = EMP3.DEPT_ID
                       and EMP4.SALARY >= EMP3.SALARY) RANK
                  ,(select count(*)
                      from EMP EMP2
                     where EMP2.DEPT_ID = EMP3.DEPT_ID
                       and EMP2.SALARY = EMP3.SALARY) TIE_COUNT
                  ,0 POPULATION
              from EMP EMP3
             where TOP_N >= -- Here is the meat, Correlated subquery
                   (select count(distinct EMP4.SALARY)
                      from EMP EMP4
                     where EMP4.DEPT_ID = EMP3.DEPT_ID
                       and EMP4.SALARY >= EMP3.SALARY))
     order by DEPT_ID
             ,LINE
             ,SALARY desc
             ,EMP_ID;

  V_EMP CSR_EMP%rowtype;
begin
  for V_EMP in CSR_EMP(TOP_N)
  loop
    DBMS_OUTPUT.PUT_LINE(v_emp."Top rank per group");
  end loop;
end;
```


## PowerShell


```powershell
function New-Employee ($Name, $ID, $Salary, $Department) {
    New-Object PSObject `
        | Add-Member -PassThru NoteProperty EmployeeName $Name `
        | Add-Member -PassThru NoteProperty EmployeeID $ID `
        | Add-Member -PassThru NoteProperty Salary $Salary `
        | Add-Member -PassThru NoteProperty Department $Department
}

$data = (New-Employee 'Tyler Bennett'    E10297  32000  D101),
        (New-Employee 'John Rappl'       E21437  47000  D050),
        (New-Employee 'George Woltman'   E00127  53500  D101),
        (New-Employee 'Adam Smith'       E63535  18000  D202),
        (New-Employee 'Claire Buckman'   E39876  27800  D202),
        (New-Employee 'David McClellan'  E04242  41500  D101),
        (New-Employee 'Rich Holcomb'     E01234  49500  D202),
        (New-Employee 'Nathan Adams'     E41298  21900  D050),
        (New-Employee 'Richard Potter'   E43128  15900  D101),
        (New-Employee 'David Motsinger'  E27002  19250  D202),
        (New-Employee 'Tim Sampair'      E03033  27000  D101),
        (New-Employee 'Kim Arlich'       E10001  57000  D190),
        (New-Employee 'Timothy Grove'    E16398  29900  D190)

function Get-TopRank ($n) {
    $data `
        | Group-Object Department `
        | ForEach-Object {
              $_.Group `
                  | Sort-Object Salary -Descending `
                  | Select-Object -First $n
          } `
        | Format-Table -GroupBy Department
}
```
<pre style="height:30ex;overflow:scroll">PS> Get-TopRank 2

   Department: D101

EmployeeName         EmployeeID    Salary Department
------------         ----------    ------ ----------
George Woltman       E00127         53500 D101
David McClellan      E04242         41500 D101

   Department: D050

EmployeeName         EmployeeID    Salary Department
------------         ----------    ------ ----------
John Rappl           E21437         47000 D050
Nathan Adams         E41298         21900 D050

   Department: D202

EmployeeName         EmployeeID    Salary Department
------------         ----------    ------ ----------
Rich Holcomb         E01234         49500 D202
Claire Buckman       E39876         27800 D202

   Department: D190

EmployeeName         EmployeeID    Salary Department
------------         ----------    ------ ----------
Kim Arlich           E10001         57000 D190
Timothy Grove        E16398         29900 D190
```



## Prolog


```prolog
% emp(name,id,salary,dpt)
emp('Tyler Bennett','E10297',32000,'D101').
emp('John Rappl','E21437',47000,'D050').
emp('George Woltman','E00127',53500,'D101').
emp('Adam Smith','E63535',18000,'D202').
emp('Claire Buckman','E39876',27800,'D202').
emp('David McClellan','E04242',41500,'D101').
emp('Rich Holcomb','E01234',49500,'D202').
emp('Nathan Adams','E41298',21900,'D050').
emp('Richard Potter','E43128',15900,'D101').
emp('David Motsinger','E27002',19250,'D202').
emp('Tim Sampair','E03033',27000,'D101').
emp('Kim Arlich','E10001',57000,'D190').
emp('Timothy Grove','E16398',29900,'D190').

departments(Depts) :-  % Find the set of departments
  findall(Dpt, emp(_,_,_,Dpt), DList), list_to_set(DList, Depts).

greater(emp(_,_,Sal1,_), emp(_,_,Sal2,_)) :-
  Sal1 > Sal2.  % First employee salary greater than second

% Maintains a decreasing ordered list of employees truncated after (N) items.
%  Rule 1: For N=0, always return an empty set.
%  Rule 2: Add employee with greater salary at start of list, call with N-1
%  Rule 3: Try to add new employee at N-1
%  Rule 4: for an empty input list regardless of N, add the new employee
topSalary(0, _, _, []).
topSalary(N, Emp, [E|R], [Emp|Res]) :-
  greater(Emp,E), N0 is N - 1, !, topSalary(N0, E, R, Res).
topSalary(N, Emp, [E|R], [E|Res]) :-
  N0 is N - 1, !, topSalary(N0, Emp, R, Res).
topSalary(_, Emp, [], [Emp]).

% For each employee, add him to the list if top salary
topEmps(N, [Emp|Emps], R, Res) :-
  topSalary(N, Emp, R, Rt), !, topEmps(N, Emps, Rt, Res).
topEmps(_, [], Res, Res).

% For each department, find the list of top employees in that department
topDeps(N, [Dept|T], [dept(Dept,Ro)|Res]) :-
  findall(emp(Name, Id, Sal, Dept), emp(Name, Id, Sal, Dept), Emps),
  topEmps(N, Emps, [], Ro), !, topDeps(N, T, Res).
topDeps(_, [], []).

% Calculate and report the list of highest salaried employees per department
topDeps(N) :-
  departments(D), topDeps(N, D, Res),
  member(dept(Dept,R), Res),
  writef('Department: %w\n', [Dept]),
  member(emp(Name,Id,Sal,_), R),
  writef('  ID: %w\t%w\tSalary: %w\n', [Id,Name,Sal]),
  fail.
topDeps(_).
```
<pre style="height:30ex;overflow:scroll">?- topDeps(2).
Department: D101
  ID: E00127	George Woltman	Salary: 53500
  ID: E04242	David McClellan	Salary: 41500
Department: D050
  ID: E21437	John Rappl	Salary: 47000
  ID: E41298	Nathan Adams	Salary: 21900
Department: D202
  ID: E01234	Rich Holcomb	Salary: 49500
  ID: E39876	Claire Buckman	Salary: 27800
Department: D190
  ID: E10001	Kim Arlich	Salary: 57000
  ID: E16398	Timothy Grove	Salary: 29900
true.

```



## PureBasic


```PureBasic
Structure Employees
  Name$
  ID$
  Salary.i
  Department$
EndStructure

Procedure displayTopEarners(List MyEmployees.Employees(), n)
  Protected filename$ = OpenFileRequester("Top rank per group", "DataFile.txt", "", 0)
  If ReadFile(0, filename$)
    Protected InData.Employees, txt.s, MaxNameLength

    While Eof(0) = 0
      AddElement(MyEmployees())
      txt = ReadString(0)
      With MyEmployees()
        \Name$ = StringField(txt, 1, ",")
        \ID$ = StringField(txt, 2, ",")
        \Salary = Val(StringField(txt, 3, ","))
        \Department$ = StringField(txt, 4, ",")
        If Len(\Name$) > MaxNameLength: MaxNameLength = Len(\Name$): EndIf
      EndWith
    Wend
    CloseFile(0)
  Else
    MessageRequester("Information", "Couldn't open the file!")
    End
  EndIf

  If OpenConsole()
    Protected OldDepartment$, count

    SortStructuredList(MyEmployees(), #PB_Sort_Descending, OffsetOf(Employees\Salary), #PB_Sort_integer)
    SortStructuredList(MyEmployees(), #PB_Sort_Ascending, OffsetOf(Employees\Department$), #PB_Sort_String)
    ForEach MyEmployees()
      With MyEmployees()
        If \Department$ <> OldDepartment$
          If OldDepartment$ <> ""
            PrintN(#CRLF$)
          EndIf
          OldDepartment$ = \Department$
          PrintN("Department " + \Department$ + #CRLF$ + "---------------")
          PrintN(LSet("Name", MaxNameLength + 3) + LSet("ID", 7) + LSet("Salary", 7))
          count = 0
        EndIf
        count + 1
        If count <= n
          PrintN(LSet(\Name$, MaxNameLength + 1) + " " + RSet(\ID$, 7) + " $" + Str(\Salary))
        EndIf
      EndWith
    Next
    PrintN(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  EndIf
EndProcedure

NewList MyEmployees.Employees()

displayTopEarners(MyEmployees(), 3)
```


'''Save this as 'DataFile.txt and let the program open this file'''
```txt

 Tyler Bennett,E10297,32000,D101
 John Rappl,E21437,47000,D050
 George Woltman,E00127,53500,D101
 Adam Smith,E63535,18000,D202
 Claire Buckman,E39876,27800,D202
 David McClellan,E04242,41500,D101
 Rich Holcomb,E01234,49500,D202
 Nathan Adams,E41298,21900,D050
 Richard Potter,E43128,15900,D101
 David Motsinger,E27002,19250,D202
 Tim Sampair,E03033,27000,D101
 Kim Arlich,E10001,57000,D190
 Timothy Grove,E16398,29900,D190
```
<pre style="height:30ex;overflow:scroll">Department D050
---------------
Name              ID     Salary
John Rappl        E21437 $47000
Nathan Adams      E41298 $21900


Department D101
---------------
Name              ID     Salary
George Woltman    E00127 $53500
David McClellan   E04242 $41500
Tyler Bennett     E10297 $32000


Department D190
---------------
Name              ID     Salary
Kim Arlich        E10001 $57000
Timothy Grove     E16398 $29900


Department D202
---------------
Name              ID     Salary
Rich Holcomb      E01234 $49500
Claire Buckman    E39876 $27800
David Motsinger   E27002 $19250


Press ENTER to exit
```



## Python

Python 2.7/3.x compatible.

```python
from collections import defaultdict
from heapq import nlargest

data = [('Employee Name', 'Employee ID', 'Salary', 'Department'),
        ('Tyler Bennett', 'E10297', 32000, 'D101'),
        ('John Rappl', 'E21437', 47000, 'D050'),
        ('George Woltman', 'E00127', 53500, 'D101'),
        ('Adam Smith', 'E63535', 18000, 'D202'),
        ('Claire Buckman', 'E39876', 27800, 'D202'),
        ('David McClellan', 'E04242', 41500, 'D101'),
        ('Rich Holcomb', 'E01234', 49500, 'D202'),
        ('Nathan Adams', 'E41298', 21900, 'D050'),
        ('Richard Potter', 'E43128', 15900, 'D101'),
        ('David Motsinger', 'E27002', 19250, 'D202'),
        ('Tim Sampair', 'E03033', 27000, 'D101'),
        ('Kim Arlich', 'E10001', 57000, 'D190'),
        ('Timothy Grove', 'E16398', 29900, 'D190')]

departments = defaultdict(list)
for rec in data[1:]:
    departments[rec[-1]].append(rec)

N = 3
format = " %-15s " * len(data[0])
for department, recs in sorted(departments.items()):
    print ("Department %s" % department)
    print (format % data[0])
    for rec in nlargest(N, recs, key=lambda rec: rec[-2]):
        print (format % rec)
    print('')
```
<pre style="height:30ex;overflow:scroll">Department D050
 Employee Name    Employee ID      Salary           Department
 John Rappl       E21437           47000            D050
 Nathan Adams     E41298           21900            D050

Department D101
 Employee Name    Employee ID      Salary           Department
 George Woltman   E00127           53500            D101
 David McClellan  E04242           41500            D101
 Tyler Bennett    E10297           32000            D101

Department D190
 Employee Name    Employee ID      Salary           Department
 Kim Arlich       E10001           57000            D190
 Timothy Grove    E16398           29900            D190

Department D202
 Employee Name    Employee ID      Salary           Department
 Rich Holcomb     E01234           49500            D202
 Claire Buckman   E39876           27800            D202
 David Motsinger  E27002           19250            D202

```




'''Alternative Solution'''

Uses namedtuples for database records, and groupby builtin to group records by Department:
```python
from collections import namedtuple
from itertools import groupby

N = 2

db = '''Employee Name,Employee ID,Salary,Department
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190'''

rows = db.split('\n')
DBRecord = namedtuple('DBRecord', rows[0].replace(' ', '_'))

records = [DBRecord(*row.split(',')) for row in rows[1:]]

records.sort(key=lambda record: (record.Department, -float(record.Salary)))

print('\n\n'.join(
    '\n  '.join([dpt] + [str(g) for g in grp][:N])
    for dpt, grp in groupby(
        records,
        lambda record: record.Department
    )
))
```
<pre style="height:30ex;overflow:scroll">D050
  DBRecord(Employee_Name='John Rappl', Employee_ID='E21437', Salary='47000', Department='D050')
  DBRecord(Employee_Name='Nathan Adams', Employee_ID='E41298', Salary='21900', Department='D050')

D101
  DBRecord(Employee_Name='George Woltman', Employee_ID='E00127', Salary='53500', Department='D101')
  DBRecord(Employee_Name='David McClellan', Employee_ID='E04242', Salary='41500', Department='D101')

D190
  DBRecord(Employee_Name='Kim Arlich', Employee_ID='E10001', Salary='57000', Department='D190')
  DBRecord(Employee_Name='Timothy Grove', Employee_ID='E16398', Salary='29900', Department='D190')

D202
  DBRecord(Employee_Name='Rich Holcomb', Employee_ID='E01234', Salary='49500', Department='D202')
  DBRecord(Employee_Name='Claire Buckman', Employee_ID='E39876', Salary='27800', Department='D202')
```



## R

First, read in the data.

```R
dfr <- read.csv(tc <- textConnection(
"Employee Name,Employee ID,Salary,Department
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190")); close(tc)
```

To just return the top salary, it's very simple using tapply.

```R
with(dfr, tapply(Salary, Department, max))
```

To return N salaries, we replace max with our own function.

```R
get.top.N.salaries <- function(N)
{
   with(dfr, tapply(Salary, Department,
      function(x)
      {
         sort(x);
         lx <- length(x)
         if(N >= lx) return(x)
         x[-1:(N-lx)]
      }))
}

get.top.N.salaries(3)
```

 $D050
 [1] 47000 21900

 $D101
 [1] 41500 15900 27000

 $D190
 [1] 57000 29900

 $D202
 [1] 27800 49500 19250

To return the whole record for each of the top salaries, a different tack is required.

```R
get.top.N.salaries2 <- function(N)
{
   #Sort data frame by Department, then by Salary
   sorted <- dfr[with(dfr, order(Department, Salary, decreasing=TRUE)),]
   #Split the dataframe up, by Department
   bydept <- split(sorted, sorted$Department)
   #Return the first N values (or all of them
   lapply(bydept,
      function(x)
      {
         n <- min(N, nrow(x))
         x[1:n,]
      })
}
get.top.N.salaries2(3)
```
<pre style="height:30ex;overflow:scroll"> $D050
   Employee.Name Employee.ID Salary Department
 2    John Rappl      E21437  47000       D050
 8  Nathan Adams      E41298  21900       D050

 $D101
     Employee.Name Employee.ID Salary Department
 3  George Woltman      E00127  53500       D101
 6 David McClellan      E04242  41500       D101
 1   Tyler Bennett      E10297  32000       D101

 $D190
    Employee.Name Employee.ID Salary Department
 12    Kim Arlich      E10001  57000       D190
 13 Timothy Grove      E16398  29900       D190

 $D202
      Employee.Name Employee.ID Salary Department
 7     Rich Holcomb      E01234  49500       D202
 5   Claire Buckman      E39876  27800       D202
 10 David Motsinger      E27002  19250       D202
```



###  With dplyr

With the dplyr package we can use group_by and top_n to select the top n records in each group.


```R

library(dplyr)
dfr %>%
  group_by(Department) %>%
  top_n(2, Salary)

```

Provides:
<pre style="height:30ex;overflow:scroll">
Employee.Name   Employee.ID Salary Department
  <fct>           <fct>        <int> <fct>
John Rappl      E21437       47000 D050
George Woltman  E00127       53500 D101
Claire Buckman  E39876       27800 D202
David McClellan E04242       41500 D101
Rich Holcomb    E01234       49500 D202
Nathan Adams    E41298       21900 D050
Kim Arlich      E10001       57000 D190
Timothy Grove   E16398       29900 D190

```



## Racket


```Racket
#lang racket

(struct employee (name id salary dept))
(define employees
  (list (employee "Tyler Bennett"   "E10297" 32000 "D101")
        (employee "John Rappl"      "E21437" 47000 "D050")
        (employee "George Woltman"  "E00127" 53500 "D101")
        (employee "Adam Smith"      "E63535" 18000 "D202")
        (employee "Claire Buckman"  "E39876" 27800 "D202")
        (employee "David McClellan" "E04242" 41500 "D101")
        (employee "Rich Holcomb"    "E01234" 49500 "D202")
        (employee "Nathan Adams"    "E41298" 21900 "D050")
        (employee "Richard Potter"  "E43128" 15900 "D101")
        (employee "David Motsinger" "E27002" 19250 "D202")
        (employee "Tim Sampair"     "E03033" 27000 "D101")
        (employee "Kim Arlich"      "E10001" 57000 "D190")
        (employee "Timothy Grove"   "E16398" 29900 "D190")))

(define (top/dept N)
  (for/list ([dept (remove-duplicates (map employee-dept employees))])
    (define people
      (filter (λ(e) (equal? dept (employee-dept e))) employees))
    (cons dept (take (sort people > #:key employee-salary) N))))

(for ([dept (top/dept 2)])
  (printf "Department ~a:\n" (car dept))
  (for ([e (cdr dept)])
    (printf "  $~a: ~a (~a)\n"
            (employee-salary e)
            (employee-name e)
            (employee-id e))))

```
<pre style="height:30ex;overflow:scroll">Department D101:
  $53500: George Woltman (E00127)
  $41500: David McClellan (E04242)
Department D050:
  $47000: John Rappl (E21437)
  $21900: Nathan Adams (E41298)
Department D202:
  $49500: Rich Holcomb (E01234)
  $27800: Claire Buckman (E39876)
Department D190:
  $57000: Kim Arlich (E10001)
  $29900: Timothy Grove (E16398)
```



## REXX


### version 1


```rexx
/*REXX program displays the top   N   salaries in each department (internal table).     */
parse arg topN .                                 /*get optional # for the top N salaries*/
if topN=='' | topN==","  then topN=1             /*Not specified?  Then use the default.*/
say 'Finding the top '   topN   " salaries in each department.";            say
@.=      /*════════ employee name      ID    salary   dept. ═══════ */
            @.1 = "Tyler Bennett    ,E10297,  32000,  D101"
            @.2 = "John Rappl       ,E21437,  47000,  D050"
            @.3 = "George Woltman   ,E00127,  53500,  D101"
            @.4 = "Adam Smith       ,E63535,  18000,  D202"
            @.5 = "Claire Buckman   ,E39876,  27800,  D202"
            @.6 = "David McClellan  ,E04242,  41500,  D101"
            @.7 = "Rich Holcomb     ,E01234,  49500,  D202"
            @.8 = "Nathan Adams     ,E41298,  21900,  D050"
            @.9 = "Richard Potter   ,E43128,  15900,  D101"
           @.10 = "David Motsinger  ,E27002,  19250,  D202"
           @.11 = "Tim Sampair      ,E03033,  27000,  D101"
           @.12 = "Kim Arlich       ,E10001,  57000,  D190"
           @.13 = "Timothy Grove    ,E16398,  29900,  D190"
depts=
                   do j=1  until @.j==''         /*build database elements from @ array.*/
                   parse var  @.j  name.j    ','    id.j    ","    sal.j    ','   dept.j .
                   if wordpos(dept.j,depts)==0  then depts=depts dept.j    /*a new DEPT?*/
                   end   /*j*/
employees=j-1                                    /*adjust for the  DO  loop  index bump.*/
say 'There are '   employees   "employees, "    words(depts)     'departments: '     depts
say
    do dep=1  for words(depts);      say         /*process each of the departments.     */
    Xdept=word(depts,dep)                        /*current department being processed.  */
        do topN;             highSal=0           /*process the top  N  salaries.        */
        h=0                                      /*point to the highest paid employee.  */
            do e=1  for employees                /*process each employee in department. */
            if dept.e\==Xdept | sal.e<highSal  then iterate    /*is this the wrong info?*/
            highSal=sal.e;   h=e                 /*a higher salary was just discovered. */
            end   /*e*/

        if h==0  then iterate                    /*do we have no highest paid this time?*/
        say 'department:  '      dept.h      " $" || sal.h+0       id.h      space(name.h)
        dept.h=                                  /*make sure we see the employee again. */
        end       /*topN*/
    end           /*dep*/                        /*stick a fork in it,  we're all done. */
```

'''output'''   when using the input:   <tt> 2 </tt>

```txt

Finding the top  2  salaries in each department.

There are  13 employees,  4 departments:   D101 D050 D202 D190


department:   D101  $53500 E00127 George Woltman
department:   D101  $41500 E04242 David McClellan

department:   D050  $47000 E21437 John Rappl
department:   D050  $21900 E41298 Nathan Adams

department:   D202  $49500 E01234 Rich Holcomb
department:   D202  $27800 E39876 Claire Buckman

department:   D190  $57000 E10001 Kim Arlich
department:   D190  $29900 E16398 Timothy Grove

```

'''output'''   when using the input:   <tt> 100 </tt>

```txt

Finding the top  100  salaries in each department.

There are  13 employees,  4 departments:   D101 D050 D202 D190


department:   D101  $53500 E00127 George Woltman
department:   D101  $41500 E04242 David McClellan
department:   D101  $32000 E10297 Tyler Bennett
department:   D101  $27000 E03033 Tim Sampair
department:   D101  $15900 E43128 Richard Potter

department:   D050  $47000 E21437 John Rappl
department:   D050  $21900 E41298 Nathan Adams

department:   D202  $49500 E01234 Rich Holcomb
department:   D202  $27800 E39876 Claire Buckman
department:   D202  $19250 E27002 David Motsinger
department:   D202  $18000 E63535 Adam Smith

department:   D190  $57000 E10001 Kim Arlich
department:   D190  $29900 E16398 Timothy Grove

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 12.02.2014 Walter Pachl
*--------------------------------------------------------------------*/
  Parse Arg topn .                 /* get number for top N salaries. */
  Select
    When topn='' Then              /* no argument                    */
      topn=1                       /* assume only  1.                */
    When topn='?' Then             /* user wants help                */
      Call help
    When datatype(topn)<>'NUM' Then /* Argument is not a number      */
      Call exit 'Invalid argument ('topn'). Must be a number!'
    Otherwise
      Nop
    End
Parse Value '0 0 0 0' with en dn esal. de. deptl
         /*employee name,  ID, salary, dept.*/
Call read "Tyler Bennett,E10297,32000,D101"
Call read "George Woltman,E00127,53500,D101"
Call read "John Rappl,E21437,47000,D050"
Call read "Adam Smith,E63535,18000,D202"
Call read "Claire Buckman,E39876,27800,D202"
Call read "David McClellan,E04242,41500,D101"
Call read "Rich Holcomb,E01234,49500,D202"
Call read "Nathan Adams,E41298,21900,D050"
Call read "Richard Potter,E43128,15900,D101"
Call read "David Motsinger,E27002,19250,D202"
Call read "Tim Sampair,E03033,27000,D101"
Call read "Kim Arlich,E10001,57000,D190"
Call read "Timothy Grove,E16398,29900,D190"
Say en 'employees,' dn "departments:" deptl
Do e=1 To en
  d=dept.e
  Do di=1 To de.d
    If esal.d.di<sal.e Then
      Leave
    End
  Do j=de.d To di By -1
    j1=j+1
    esal.d.j1=esal.d.j
    enum.d.j1=enum.d.j
    End
  esal.d.di=sal.e
  enum.d.di=id.e
  de.d=de.d+1
  End
/*---------------------------------------------------------------------
* Output
*--------------------------------------------------------------------*/
Say ' '
Say 'Showing top' topn 'salaries in each department.'
Say ' '
Do While deptl<>''
  Parse Var deptl d deptl
  Do i=1 To min(topn,de.d)
    id=enum.d.i
    Say 'department:  'd'  $'esal.d.i id name.id
    End
  Say ' '
  End
Exit

read:
en=en+1
Parse Arg name ',' id.en "," sal.en ',' dept.en
If wordpos(dept.en,deptl)=0 Then Do
  dn=dn+1
  deptl=deptl dept.en
  End
z=id.en
name.z=name
Return

exit: Say arg(1)
help: Say 'Syntax: rexx topsal [topn]'
      Exit
```

'''output'''

```txt
13 employees, 4 departments:  D101 D050 D202 D190

Showing top 2 salaries in each department.

department:  D101  $53500 E00127 George Woltman
department:  D101  $41500 E04242 David McClellan

department:  D050  $47000 E21437 John Rappl
department:  D050  $21900 E41298 Nathan Adams

department:  D202  $49500 E01234 Rich Holcomb
department:  D202  $27800 E39876 Claire Buckman

department:  D190  $57000 E10001 Kim Arlich
department:  D190  $29900 E16398 Timothy Grove
```



## Ring


```ring

# Project : Top rank per group

load "stdlib.ring"
salary = "Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190"

temp = substr(salary, ",", nl)
temp = str2list(temp)
depsal = newlist(13,4)
for n = 1 to len(temp)
     n1 = ceil(n/4)
     n2 = n%4
     if n2 = 0
        n2 = 4
     ok
     depsal[n1][n2] = temp[n]
next
for n = 1 to len(depsal)-1
     for m = n+1 to len(depsal)
          if strcmp(depsal[m][4], depsal[n][4]) < 0
              tmp = depsal[n]
              depsal[n] = depsal[m]
              depsal[m] = tmp
          ok
      next
next
for n = 1 to len(depsal)-1
     for m = n+1 to len(depsal)
           if (depsal[m][4] = depsal[n][4]) and (depsal[m][3] > depsal[n][3])
               tmp = depsal[n]
               depsal[n] = depsal[m]
               depsal[m] = tmp
           ok
      next
next
see "Department : " + depsal[1][4] + nl
see "Name                   " + "Id             " + "Salary" + nl + nl
see "" + depsal[1][1] + "      " + depsal[1][2] + "      " + depsal[1][3]+ nl
for n = 1 to len(depsal)-1
    if (depsal[n+1][4] != depsal[n][4])
        see nl
        see "Department : " + depsal[n+1][4] + nl
        see "Name                   " + "Id             " + "Salary" + nl + nl
        see "" + depsal[n+1][1] + "      " + depsal[n+1][2] + "      " + depsal[n+1][3]+ nl
     else
        see "" + depsal[n+1][1] + "      " + depsal[n+1][2] + "      " + depsal[n+1][3]+ nl
     ok
next

```

Output:

```txt

Department : D050
Name                   Id             Salary

John Rappl             E21437         47000
Nathan Adams           E41298         21900

Department : D101
Name                   Id             Salary

George Woltman         E00127         53500
David McClellan        E04242         41500
Tyler Bennett          E10297         32000
Tim Sampair            E03033         27000
Richard Potter         E43128         15900

Department : D190
Name                   Id             Salary

Kim Arlich             E10001         57000
Timothy Grove          E16398         29900

Department : D202
Name                   Id             Salary

Rich Holcomb           E01234         49500
Claire Buckman         E39876         27800
David Motsinger        E27002         19250
Adam Smith             E63535         18000

```



## Ruby

Without much thought to report formatting:
```ruby
require "csv"

data = <<EOS
Employee Name,Employee ID,Salary,Department
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190
EOS

def show_top_salaries_per_group(data, n)
  table = CSV.parse(data, :headers=>true, :header_converters=>:symbol)
  groups = table.group_by{|emp| emp[:department]}.sort
  groups.each do |dept, emps|
    puts dept
    # max by salary
    emps.max_by(n) {|emp| emp[:salary].to_i}.each do |e|
      puts "    %-16s %6s %7d" % [e[:employee_name], e[:employee_id], e[:salary]]
    end
    puts
  end
end

show_top_salaries_per_group(data, 3)
```


```txt

D050
    John Rappl       E21437   47000
    Nathan Adams     E41298   21900

D101
    George Woltman   E00127   53500
    David McClellan  E04242   41500
    Tyler Bennett    E10297   32000

D190
    Kim Arlich       E10001   57000
    Timothy Grove    E16398   29900

D202
    Rich Holcomb     E01234   49500
    Claire Buckman   E39876   27800
    David Motsinger  E27002   19250

```



## Run BASIC


```runbasic
perSal$ = "Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050;
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202;
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190"

while word$(perSal$,n+1,chr$(13)) <> "" : n = n + 1 : wend   ' get count of employees
dim depSal$(n)
for i = 1 to n
  depSal$(i) = word$(perSal$,i,chr$(13))
next i
sw = 1
while sw = 1
  sw = 0
  for i = 1 to n -1
    if word$(depSal$(i),4,",")+word$(depSal$(i),3,",") > word$(depSal$(i+1),4,",")+word$(depSal$(i+1),3,",") then
      temp$        = depSal$(i)
      depSal$(i)   = depSal$(i+1)
      depSal$(i+1) = temp$
      sw           = 1
    end if
  next i
wend
print "Employee Name";chr$(9);"ID";chr$(9);"Salary"
for i = 1 to n
  if dep$ <> word$(depSal$(i),4,",") then
    dep$ = word$(depSal$(i),4,",")
    print : print"Department:";dep$
  end if
print word$(depSal$(i),1,",");chr$(9);word$(depSal$(i),2,",");chr$(9);word$(depSal$(i),3,",")
next i
```
<pre style="height:30ex;overflow:scroll">Employee Name	ID	Salary

Department:D050
Nathan Adams	E41298	21900

Department:D050;
John Rappl	E21437	47000

Department:D101
Richard Potter	E43128	15900
Tim Sampair	E03033	27000
Tyler Bennett	E10297	32000
David McClellan	E04242	41500
George Woltman	E00127	53500

Department:D190
Timothy Grove	E16398	29900
Kim Arlich	E10001	57000

Department:D202
David Motsinger	E27002	19250
Claire Buckman	E39876	27800
Rich Holcomb	E01234	49500

Department:D202;
Adam Smith	E63535	18000
```



## Scala


### Application idiomatic version


```scala
import scala.io.Source
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.collection.immutable.TreeMap

object TopRank extends App {
  val topN = 3

  val rawData = """Employee Name;Employee ID;Salary;Department
			   |Tyler Bennett;E10297;32000;D101
			   |John Rappl;E21437;47000;D050
			   |George Woltman;E00127;53500;D101
			   |Adam Smith;E63535;18000;D202
			   |Claire Buckman;E39876;27800;D202
			   |David McClellan;E04242;41500;D101
			   |Rich Holcomb;E01234;49500;D202
			   |Nathan Adams;E41298;21900;D050
			   |Richard Potter;E43128;15900;D101
			   |David Motsinger;E27002;19250;D202
			   |Tim Sampair;E03033;27000;D101
			   |Kim Arlich;E10001;57000;D190
			   |Timothy Grove;E16398;29900;D190""".stripMargin

  class Employee(name: String, id: String,
                 val salary: Int,
                 val department: String) {
    override def toString = s"$id\t$salary\t$name"
  }

  // A TreeMap has sorted keys
  val data: TreeMap[String, Seq[TopRank.Employee]] = // TreeMap is a sorted map
    TreeMap((Source.fromString(rawData) getLines ()).toSeq // Runtime parsing
      .drop(1) // Drop header
      .map(_.split(";")) //read fields into list of employees
      .map(emp => new Employee(emp(0), emp(1), emp(2).toInt, emp(3)))
      .groupBy(_.department).toSeq: _*)

  implicit def iterableWithAvg[T: Numeric](data: Iterable[T]) = new {
    def average[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
      num.toDouble(ts.sum) / ts.size
    }
    def avg = average(data)
  }

  val a = data.flatMap { case (_, emps) => emps.map(_.salary) }.avg

  println(s"Reporting top $topN salaries in each department.\n")

  println(s"Total of ${data.foldLeft(0)(_ + _._2.size)} employees in ${data.size} departments")

  println(f"Average salary: $a%8.2f\n")

  data.foreach {
    case (dep, emps) => println(f"Department: $dep  pop: ${emps.size} avg: ${emps.map(_.salary).avg}%8.2f\n"
      + emps.sortBy(-_.salary).take(topN)
      .map(_.toString).mkString("\t", "\n\t", ""))
  }
}
```
<pre style="height:30ex;overflow:scroll">Reporting top 3 salaries in each department.

Total of 13 employees in 4 departments
Average salary: 33865,38

Department: D190  pop: 2 avg: 43450,00
	E10001	57000	Kim Arlich
	E16398	29900	Timothy Grove
Department: D050  pop: 2 avg: 34450,00
	E21437	47000	John Rappl
	E41298	21900	Nathan Adams
Department: D101  pop: 5 avg: 33980,00
	E00127	53500	George Woltman
	E04242	41500	David McClellan
	E10297	32000	Tyler Bennett
Department: D202  pop: 4 avg: 28637,50
	E01234	49500	Rich Holcomb
	E39876	27800	Claire Buckman
	E27002	19250	David Motsinger

```


### using SLICK ORM

{{libheader|H2 Database Engine}} Version 1.3.1

```scala
import scala.slick.driver.H2Driver.simple._
import scala.slick.lifted.ProvenShape

// A Employees table with 4 columns: Employee ID, Employee Name, Department, Salary
class Emp(tag: Tag) extends Table[(String, String, String, Double)](tag, "EMP") {
  def id: Column[String] = column[String]("EMP_ID", O.PrimaryKey) // This is the primary key column
  def name: Column[String] = column[String]("EMP_NAME", O.NotNull)
  def deptId: Column[String] = column[String]("DEPT_ID", O.NotNull)
  def salary: Column[Double] = column[Double]("SALARY", O.NotNull)

  // Every table needs a * projection with the same type as the table's type parameter
  def * : ProvenShape[(String, String, String, Double)] = (id, name, deptId, salary)
}

// The main application
object TopNrankSLICK extends App {

  val topN = 3

  // The query interface for the Emp table
  val employees = TableQuery[Emp]

  // Create a connection (called a "session") to an in-memory H2 database
  Database.forURL("jdbc:h2:mem:hello", driver = "org.h2.Driver").withSession {
    implicit session =>

      // Create the schema
      employees.ddl.create

      // Fill the database
      val employeesInsertResult: Option[Int] = employees ++= Seq(
        ("E10297", "Tyler Bennett", "D101", 32000),
        ("E21437", "John Rappl", "D050", 47000),
        ("E00127", "George Woltman", "D101", 53500),
        ("E63535", "Adam Smith", "D202", 18000),
        ("E39876", "Claire Buckman", "D202", 27800),
        ("E04242", "David McClellan", "D101", 41500),
        ("E01234", "Rich Holcomb", "D202", 49500),
        ("E41298", "Nathan Adams", "D050", 21900),
        ("E43128", "Richard Potter", "D101", 15900),
        ("E27002", "David Motsinger", "D202", 19250),
        ("E03033", "Tim Sampair", "D101", 27000),
        ("E10001", "Kim Arlich", "D190", 57000),
        ("E16398", "Timothy Grove", "D190", 29900),
        ("E16399", "Timothy Grave", "D190", 29900),
        ("E16400", "Timothy Grive", "D190", 29900))

      /* Manual SQL / String Interpolation */
      // Required import for the sql interpolator
      import scala.slick.jdbc.StaticQuery.interpolation

      // Construct a SQL statement manually with an interpolated value
      val plainQuery = // First the bun - formatting SELECT clause
        sql"""select case LINE
         when 10 then
          'Tot.' || LPAD(POPULATION, 2) || ' Employees in ' || TIE_COUNT ||
          ' deps.Avg salary:' || TO_CHAR(SALARY, '99990.99')
         when 30 then
          '-'
         when 50 then
          'Department: ' || DEPT_ID || ', pop: ' || POPULATION ||
          '. Avg Salary: ' || TO_CHAR(SALARY, '99990.99')
         when 70 then
          LPAD('Employee ID', 14) || LPAD('Employee name', 20) ||
          LPAD('Salary', 9) || 'Rank'
         when 90 then
          LPAD('+', 14, '-') || LPAD('+', 20, '-') || LPAD('+', 9, '-') ||
          LPAD('+', 4, '-')
         else
          LPAD(' ', 8) || LPAD(EMP_ID, 6) || LPAD(EMP_NAME, 20) ||
          TO_CHAR(SALARY, '99990.99') || LPAD(case when TIE_COUNT = 1 then ' ' else 'T' end || RANK, 4)
       end "Top rank per group"
  from (select 10 LINE
              ,null EMP_ID
              ,null EMP_NAME
              ,' ' DEPT_ID
              ,avg(SALARY) SALARY
              ,0 RANK
              ,count(distinct DEPT_ID) TIE_COUNT
              ,count(*) POPULATION
          from EMP
        union all
        select 30      LINE
              ,null    EMP_ID
              ,null    EMP_NAME
              ,DEPT_ID
              ,0       SALARY
              ,0       RANK
              ,0       TIE_COUNT
              ,0       POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 50 LINE
              ,null EMP_ID
              ,null EMP_NAME
              ,DEPT_ID
              ,avg(SALARY) SALARY
              ,0 RANK
              ,0 TIE_COUNT
              ,count(*) POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 70      LINE
              ,null    EMP_ID
              ,null    EMP_NAME
              ,DEPT_ID
              ,0       SALARY
              ,0       RANK
              ,0       TIE_COUNT
              ,0       POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 90      LINE
              ,null    EMP_ID
              ,null    EMP_NAME
              ,DEPT_ID
              ,0       SALARY
              ,0       RANK
              ,0       TIE_COUNT
              ,0       POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 110 LINE
              ,EMP_ID
              ,EMP_NAME
              ,DEPT_ID
              ,SALARY
              ,(select count(distinct EMP4.SALARY)
                  from EMP EMP4
                 where EMP4.DEPT_ID = EMP3.DEPT_ID
                   and EMP4.SALARY >= EMP3.SALARY) RANK
              ,(select count(*)
                  from EMP EMP2
                 where EMP2.DEPT_ID = EMP3.DEPT_ID
                   and EMP2.SALARY = EMP3.SALARY) TIE_COUNT
              ,0 POPULATION
          from EMP EMP3
         where $topN >= -- Here is the meat, Correlated subquery
               (select count(distinct EMP4.SALARY)
                  from EMP EMP4
                 where EMP4.DEPT_ID = EMP3.DEPT_ID
                   and EMP4.SALARY >= EMP3.SALARY))
 order by DEPT_ID ,LINE ,SALARY desc, EMP_ID""".as[String]

      // Execute the query
      plainQuery.foreach(println(_))
  } // session
} // TopNrankSLICK
```

<pre style="height:30ex;overflow:scroll">Tot.15 Employees in 4 deps.Avg salary: 33336.67
-
Department: D050, pop: 2. Avg Salary:  34450.00
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E21437          John Rappl 47000.00   1
        E41298        Nathan Adams 21900.00   2
-
Department: D101, pop: 5. Avg Salary:  33980.00
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E00127      George Woltman 53500.00   1
        E04242     David McClellan 41500.00   2
        E10297       Tyler Bennett 32000.00   3
-
Department: D190, pop: 4. Avg Salary:  36675.00
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E10001          Kim Arlich 57000.00   1
        E16398       Timothy Grove 29900.00  T2
        E16399       Timothy Grave 29900.00  T2
        E16400       Timothy Grive 29900.00  T2
-
Department: D202, pop: 4. Avg Salary:  28637.50
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E01234        Rich Holcomb 49500.00   1
        E39876      Claire Buckman 27800.00   2
        E27002     David Motsinger 19250.00   3

```




## Scheme

```Scheme
(use gauche.record)

;;  This will let us treat a list as though it is a structure (record).
(define-record-type (employee (pseudo-rtd <list>)) #t #t
  name id salary dept)

(define (get-fields str)
  (map (^x (if (#/^\d/ x) (string->number x) x))
       (string-split str #\,)))

(define (print-record column-widths record)
  (display "  ")
  (print (string-join
    (map
      (^(x width)
        (if (number? x)
          (format "~vD" width x)
          (format "~vA" width x)))
      record
      column-widths))))

(define (get-column-widths records)
  (apply
    map
      (lambda column
        (apply max (map (compose string-length x->string) column)))
      records))

(define records
  (map get-fields
    (string-split
      "Tyler Bennett,E10297,32000,D101
      John Rappl,E21437,47000,D050
      George Woltman,E00127,53500,D101
      Adam Smith,E63535,18000,D202
      Claire Buckman,E39876,27800,D202
      David McClellan,E04242,41500,D101
      Rich Holcomb,E01234,49500,D202
      Nathan Adams,E41298,21900,D050
      Richard Potter,E43128,15900,D101
      David Motsinger,E27002,19250,D202
      Tim Sampair,E03033,27000,D101
      Kim Arlich,E10001,57000,D190
      Timothy Grove,E16398,29900,D190"
      #/\s*\n\s*/)))

(define (top-salaries n records)
  (let ((departments (sort (delete-duplicates (map employee-dept records))))
        (col-widths (get-column-widths records))
        (sorted-by-salary (sort records > employee-salary)))
    (dolist (dept departments)
      (print dept)
      (let1 matches (filter (^x (string=? dept (employee-dept x)))
                            sorted-by-salary)
        (for-each
          (pa$ print-record col-widths)
          (take* matches n))))))
```

<b>Testing:</b>

```txt

gosh> (top-salaries 0 records)
D050
D101
D190
D202

gosh> (top-salaries 2 records)
D050
  John Rappl      E21437 47000 D050
  Nathan Adams    E41298 21900 D050
D101
  George Woltman  E00127 53500 D101
  David McClellan E04242 41500 D101
D190
  Kim Arlich      E10001 57000 D190
  Timothy Grove   E16398 29900 D190
D202
  Rich Holcomb    E01234 49500 D202
  Claire Buckman  E39876 27800 D202

gosh> (top-salaries 999000 records)
D050
  John Rappl      E21437 47000 D050
  Nathan Adams    E41298 21900 D050
D101
  George Woltman  E00127 53500 D101
  David McClellan E04242 41500 D101
  Tyler Bennett   E10297 32000 D101
  Tim Sampair     E03033 27000 D101
  Richard Potter  E43128 15900 D101
D190
  Kim Arlich      E10001 57000 D190
  Timothy Grove   E16398 29900 D190
D202
  Rich Holcomb    E01234 49500 D202
  Claire Buckman  E39876 27800 D202
  David Motsinger E27002 19250 D202
  Adam Smith      E63535 18000 D202

```



## Sidef

```ruby
var data = <<'EOF'.lines.map{ <name id salary dept> ~Z .split(',') -> flat.to_h }
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190
EOF

var n = (ARGV ? Num(ARGV[0]) : "usage: #{__MAIN__} [n]\n".die)

for d in (data.map {|h| h{:dept} }.uniq.sort) {
    var es = data.grep { _{:dept} == d }.sort_by { -Num(_{:salary}) }
    say d
    n.times {
        es || break
        printf("%-15s | %-6s | %5d\n", es.shift(){<name id salary>...})
    }
    print "\n"
}
```

```txt

$ sidef top_rank.sf 3
D050
John Rappl      | E21437 | 47000
Nathan Adams    | E41298 | 21900

D101
George Woltman  | E00127 | 53500
David McClellan | E04242 | 41500
Tyler Bennett   | E10297 | 32000

D190
Kim Arlich      | E10001 | 57000
Timothy Grove   | E16398 | 29900

D202
Rich Holcomb    | E01234 | 49500
Claire Buckman  | E39876 | 27800
David Motsinger | E27002 | 19250

```



## SMEQL

The following [[SMEQL]] example returns the top 6 earners in each department based on this table schema:


```smeql
table: Employees
----------------
empID
dept
empName
salary
```

Source Code:

```smeql
srt = orderBy(Employees, [dept, salary], order)
top = group(srt, [(dept) dept2, max(order) order])
join(srt, top, a.dept=b.dept2 and b.order - a.order < 6)
```


Note: Since SMEQL is a query language, it has no built-in I/O capability to prompt the user for the threshold number ("6" in this example). However, it would be possible to retrieve the threshold from another table, but would make the example a bit confusing. In practice, an application language would most likely pass the value to it as a parameter.


## SQL


```sql
create table EMP
(
EMP_ID  varchar2(6 char),
EMP_NAMEvarchar2(20 char),
DEPT_ID varchar2(4 char),
SALARY  number(10,2)
);

insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E21437','John Rappl','D050',47000);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E10297','Tyler Bennett','D101',32000);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E00127','George Woltman','D101',53500);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E63535','Adam Smith','D202',18000);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E39876','Claire Buckman','D202',27800);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E04242','David McClellan','D101',41500);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E41298','Nathan Adams','D050',21900);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E43128','Richard Potter','D101',15900);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E27002','David Motsinger','D202',19250);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E03033','Tim Sampair','D101',27000);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E10001','Kim Arlich','D190',57000);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E16398','Timothy Grove','D190',29900);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E01234','Rich Holcomb','D202',49500);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E16399','Timothy Grave','D190',29900);
insert into EMP (EMP_ID, EMP_NAME, DEPT_ID, SALARY)
 values ('E16400','Timothy Grive','D190',29900);
COMMIT;
```


```sql
select case LINE
         when 10 then
          'Tot.' || LPAD(POPULATION, 2) || ' Employees in ' || TIE_COUNT ||
          ' deps.Avg salary:' || TO_CHAR(SALARY, '99990.99')
         when 30 then
          '-'
         when 50 then
          'Department: ' || DEPT_ID || ', pop: ' || POPULATION ||
          '. Avg Salary: ' || TO_CHAR(SALARY, '99990.99')
         when 70 then
          LPAD('Employee ID', 14) || LPAD('Employee name', 20) ||
          LPAD('Salary', 9) || 'Rank'
         when 90 then
          LPAD('+', 14, '-') || LPAD('+', 20, '-') || LPAD('+', 9, '-') ||
          LPAD('+', 4, '-')
         else
          LPAD(' ', 8) || LPAD(EMP_ID, 6) || LPAD(EMP_NAME, 20) ||
          TO_CHAR(SALARY, '99990.99') || LPAD(case when TIE_COUNT = 1 then  ' ' else 'T' end || RANK, 4)
       end "Top rank per group"
  from (select 10 LINE
              ,null EMP_ID
              ,null EMP_NAME
              ,' ' DEPT_ID
              ,avg(SALARY) SALARY
              ,0 RANK
              ,count(distinct DEPT_ID) TIE_COUNT
              ,count(*) POPULATION
          from EMP
        union all
        select 30      LINE
              ,null    EMP_ID
              ,null    EMP_NAME
              ,DEPT_ID
              ,0       SALARY
              ,0       RANK
              ,0       TIE_COUNT
              ,0       POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 50 LINE
              ,null EMP_ID
              ,null EMP_NAME
              ,DEPT_ID
              ,avg(SALARY) SALARY
              ,0 RANK
              ,0 TIE_COUNT
              ,count(*) POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 70      LINE
              ,null    EMP_ID
              ,null    EMP_NAME
              ,DEPT_ID
              ,0       SALARY
              ,0       RANK
              ,0       TIE_COUNT
              ,0       POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 90      LINE
              ,null    EMP_ID
              ,null    EMP_NAME
              ,DEPT_ID
              ,0       SALARY
              ,0       RANK
              ,0       TIE_COUNT
              ,0       POPULATION
          from EMP
         group by DEPT_ID
        union all
        select 110 LINE
              ,EMP_ID
              ,EMP_NAME
              ,DEPT_ID
              ,SALARY
              ,(select count(distinct EMP4.SALARY)
                  from EMP EMP4
                 where EMP4.DEPT_ID = EMP3.DEPT_ID
                   and EMP4.SALARY >= EMP3.SALARY) RANK
              ,(select count(*)
                  from EMP EMP2
                 where EMP2.DEPT_ID = EMP3.DEPT_ID
                   and EMP2.SALARY = EMP3.SALARY) TIE_COUNT
              ,0 POPULATION
          from EMP EMP3
         where $topN >= -- Here is the meat, Correlated subquery
               (select count(distinct EMP4.SALARY)
                  from EMP EMP4
                 where EMP4.DEPT_ID = EMP3.DEPT_ID
                   and EMP4.SALARY >= EMP3.SALARY))
 order by DEPT_ID ,LINE ,SALARY desc, EMP_ID;
```
<pre style="height:30ex;overflow:scroll">Tot.15 employees in 4 deps.Avg salary: 33336.67
-
Department: D050, pop: 2. Avg salary:  34450.00
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E21437          John Rappl 47000.00   1
        E41298        Nathan Adams 21900.00   2
-
Department: D101, pop: 5. Avg salary:  33980.00
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E00127      George Woltman 53500.00   1
        E04242     David McClellan 41500.00   2
        E10297       Tyler Bennett 32000.00   3
-
Department: D190, pop: 4. Avg salary:  36675.00
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E10001          Kim Arlich 57000.00   1
        E16398       Timothy Grove 29900.00  T2
        E16399       Timothy Grave 29900.00  T2
        E16400       Timothy Grive 29900.00  T2
-
Department: D202, pop: 4. Avg salary:  28637.50
   Employee ID       Employee name   SalaryRank
-------------+-------------------+--------+---+
        E01234        Rich Holcomb 49500.00   1
        E39876      Claire Buckman 27800.00   2
        E27002     David Motsinger 19250.00   3
```

Note the T2 tie ranking aka ex aequo.

Since there is no requirement for the layout - using the data from the table above

```sql
WITH ranked_emp AS (
    SELECT  emp_name
           ,dept_id
           ,salary
           ,DENSE_RANK() OVER (PARTITION BY dept_id ORDER BY salary ) ranking
    FROM    emp
    )
SELECT  dept_id
       ,ranking
       ,emp_name
       ,salary
FROM    ranked_emp
WHERE   ranking <= 2
ORDER BY dept_id, ranking;
```


<pre style="height:30ex;overflow:scroll">
DEPT    RANKING EMP_NAME                 SALARY
---- ---------- -------------------- ----------
D050          1 Nathan Adams              21900
D050          2 John Rappl                47000
D101          1 Richard Potter            15900
D101          2 Tim Sampair               27000
D190          1 Timothy Grove             29900
D190          1 Timothy Grave             29900
D190          1 Timothy Grive             29900
D190          2 Kim Arlich                57000
D202          1 Adam Smith                18000
D202          2 David Motsinger           19250
```



## Stata

The following shows the top k salaries. If there are less than k employees in a department, this will print all salaries.

```stata
import delimited employees.csv
local k 2
bysort department (salary): list salary if _N-_n<`k'
```



## Swift



```Swift
struct Employee {
  var name: String
  var id: String
  var salary: Int
  var department: String
}

let employees = [
  Employee(name: "Tyler Bennett", id: "E10297", salary: 32000, department: "D101"),
  Employee(name: "John Rappl", id: "E21437", salary: 47000, department: "D050"),
  Employee(name: "George Woltman", id: "E00127", salary: 53500, department: "D101"),
  Employee(name: "Adam Smith", id: "E63535", salary: 18000, department: "D202"),
  Employee(name: "Claire Buckman", id: "E39876", salary: 27800, department: "D202"),
  Employee(name: "David McClellan", id: "E04242", salary: 41500, department: "D101"),
  Employee(name: "Rich Holcomb", id: "E01234", salary: 49500, department: "D202"),
  Employee(name: "Nathan Adams", id: "E41298", salary: 21900, department: "D050"),
  Employee(name: "Richard Potter", id: "E43128", salary: 15900, department: "D101"),
  Employee(name: "David Motsinger", id: "E27002", salary: 19250, department: "D202"),
  Employee(name: "Tim Sampair", id: "E03033", salary: 27000, department: "D101"),
  Employee(name: "Kim Arlich", id: "E10001", salary: 57000, department: "D190"),
  Employee(name: "Timothy Grove", id: "E16398", salary: 29900, department: "D190")
]

func highestSalaries(employees: [Employee], n: Int = 1) -> [String: [Employee]] {
  return employees.reduce(into: [:], {acc, employee in
    guard var cur = acc[employee.department] else {
      acc[employee.department] = [employee]

      return
    }

    if cur.count < n {
      cur.append(employee)
    } else if cur.last!.salary < employee.salary {
      cur[n - 1] = employee
    }

    acc[employee.department] = cur.sorted(by: { $0.salary > $1.salary })
  })
}

for (dept, employees) in highestSalaries(employees: employees, n: 3) {
  let employeeString = employees.map({ "\($0.name): \($0.salary)" }).joined(separator: "\n\t")

  print("\(dept)'s highest paid employees are: \n\t\(employeeString)")
}
```


<pre style="height:30ex;overflow:scroll">D050's highest paid employees are:
	John Rappl: 47000
	Nathan Adams: 21900
D101's highest paid employees are:
	George Woltman: 53500
	David McClellan: 41500
	Tyler Bennett: 32000
D202's highest paid employees are:
	Rich Holcomb: 49500
	Claire Buckman: 27800
	David Motsinger: 19250
D190's highest paid employees are:
	Kim Arlich: 57000
	Timothy Grove: 29900
```



## Tcl

```tcl
package require Tcl 8.5

set text {Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190}

set data [dict create]
foreach line [split $text \n] {
    lassign [split $line ,] name id salary dept
    dict lappend data $dept [list $name $id $salary]
}

proc top_n_salaries {n data} {
    incr n -1
    dict for {dept employees} $data {
        puts "Department $dept"
        foreach emp [lrange [lsort -integer -decreasing -index 2 $employees] 0 $n] {
            puts [format "   %-20s %-8s %8d" {*}$emp]
        }
        puts ""
    }
}

top_n_salaries 3 $data
```
<pre style="height:30ex;overflow:scroll">Department D101
   George Woltman       E00127      53500
   David McClellan      E04242      41500
   Tyler Bennett        E10297      32000

Department D050
   John Rappl           E21437      47000
   Nathan Adams         E41298      21900

Department D202
   Rich Holcomb         E01234      49500
   Claire Buckman       E39876      27800
   David Motsinger      E27002      19250

Department D190
   Kim Arlich           E10001      57000
   Timothy Grove        E16398      29900
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
MODE DATA
$$ SET dates=*
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190
$$ MODE TUSCRIPT
SET nix=SPLIT (dates,":,:",EmployeeName,Employee_ID,Salary,Department)
SET d=MIXED_SORT (department),d=REDUCE(d)
SET index=DIGIT_INDEX(salary), index=REVERSE(index)
SET employeeName=INDEX_SORT (employeeName,index)
SET employee_ID =INDEX_SORT (employee_ID,index)
SET Salary=INDEX_SORT (salary,index)
SET Department=INDEX_SORT (Department,index)
COMPILE
LOOP l=d
PRINT "Department ", l
SET rtable=QUOTES (l)
BUILD R_TABLE pos = rtable
SET id=FILTER_INDEX (department,pos,-)
RELEASE R_TABLE pos
SET en  =SELECT (employeeName,#id)
SET ei  =SELECT (employee_ID,#id)
SET sal =SELECT (salary,#id)
SET he  =CENTER ("employeeName",-16)
SET hi  =CENTER ("employee ID",-11)
SET hs  =CENTER ("Salary",+10)
SET line=REPEAT ("-",37)
PRINT he,hi,hs
PRINT line
 LOOP e=en,i=ei,s=sal
 SET e=CENTER (e,-16), i=CENTER (i,-11), s=CENTER (s,+10)
 PRINT e,i,s
 ENDLOOP
PRINT " "
ENDLOOP
ENDCOMPILE
```
<pre style='height:30ex;overflow:scroll'>
Department D050
employeeName    employee ID    Salary
-------------------------------------
John Rappl      E21437          47000
Nathan Adams    E41298          21900

Department D101
employeeName    employee ID    Salary
-------------------------------------
George Woltman  E00127          53500
David McClellan E04242          41500
Tyler Bennett   E10297          32000
Tim Sampair     E03033          27000
Richard Potter  E43128          15900

Department D190
employeeName    employee ID    Salary
-------------------------------------
Kim Arlich      E10001          57000
Timothy Grove   E16398          29900

Department D202
employeeName    employee ID    Salary
-------------------------------------
Rich Holcomb    E01234          49500
Claire Buckman  E39876          27800
David Motsinger E27002          19250
Adam Smith      E63535          18000

```



## TXR



### Template Output Version


This version massages the data in a way that is suitable for generating the output template-wise with an <code>@(output)</code> block.

The data is in a file, exactly as given in the problem. Parameter N is accepted from command line.


```txr
@(next :args)
@{n-param}
@(next "top-rank-per-group.dat")
Employee Name,Employee ID,Salary,Department
@(collect :vars (record))
@name,@id,@salary,@dept
@(bind record (@(int-str salary) dept name id))
@(end)
@(bind (dept salary dept2 name id)
  @(let* ((n (int-str n-param))
          (dept-hash [group-by second record :equal-based])
          (dept (hash-keys dept-hash))
          (ranked (collect-each ((rec (hash-values dept-hash)))
                    [apply mapcar list [[sort rec > first] 0..n]])))
     (cons dept [apply mapcar list ranked])))
@(output)
@  (repeat)
Department: @dept
@    (repeat)
  @{name 15} (@id)  $@{salary -6}
@    (end)
@  (end)
@(end)
```
<pre style="height:30ex;overflow:scroll">Department: D101
  George Woltman  (E00127)  $ 53500
  David McClellan (E04242)  $ 41500
  Tyler Bennett   (E10297)  $ 32000
Department: D202
  Rich Holcomb    (E01234)  $ 49500
  Claire Buckman  (E39876)  $ 27800
  David Motsinger (E27002)  $ 19250
Department: D050
  John Rappl      (E21437)  $ 47000
  Nathan Adams    (E41298)  $ 21900
Department: D190
  Kim Arlich      (E10001)  $ 57000
  Timothy Grove   (E16398)  $ 29900
```


Breakdown:

Descend into argument list:


```txr
@(next :args)
```


Collect first argument as <code>n-param</code> variable:


```txr
@{n-param}
```


Drill into data file:


```txr
@(next "top-rank-per-group.dat")
```


Match header exactly:


```txr
Employee Name,Employee ID,Salary,Department
```


Now iterate over the data, requiring a variable called <code>record</code> to be bound in each iteration, and suppress all other variables from emerging.  In the body of the collect, bind four variables. Then use these four variables to create a four-element list which is bound to the variable <code>record</code>. The <code>int-str</code> function converts the textual variable <code>salary</code> to an integer:


```txr
@(collect :vars (record))
@name,@id,@salary,@dept
@(bind record (@(int-str salary) dept name id))
@(end)

```


Next, we bind five variables to the output of some TXR Lisp code, which will return five lists:


```txr
@(bind (dept salary dept2 name id)
  @(let* ((n (int-str n-param))
          (dept-hash [group-by second record :equal-based])
          (dept (hash-keys dept-hash))
          (ranked (collect-each ((rec (hash-values dept-hash)))
                    [apply mapcar list [[sort rec > first] 0..n]])))
     (cons dept [apply mapcar list ranked])))
```


This code binds some successive variables. <code>n</code> is an integer conversion of the command line argument.
<code>dept-hash</code> is a hash whose keys are department strings, and whose values are lists of records belonging to each respective department (the records collected previously).  The hash keys are the departments; these are extracted into a variable called <code>dept</code> for later use. The <code>ranked</code> variable takes the ranking information.

The salary ranking info is obtained by sorting each department's records by descending salary and then taking a 0..n slice of the list.

The "apply mapcar list" is a Lisp pattern for doing a matrix transpose.   We use it twice: once within the department over the list of records, and then over the list of lists of records.

The reason for these transpositions is to convert the data into individual nested lists, once for each field. This is the format needed by the TXR <code>@(output)</code> clause:


```txr
@(output)
@  (repeat)
Department: @dept
@    (repeat)
  @{name 15} (@id)  $@{salary -6}
@    (end)
@  (end)
@(end)
```


Here, all these variables are individual lists. The <code>dept</code> variable is a flat list; one nesting of <code>@(repeat)</code> iterates over it. The other variables are nested lists; a nested repeat drills into these.


### Lisp Output Version


In this version, the Lisp processing block performs the output, so the conversion of records into lists for the template language is omitted, simplifying the code.

The output is identical to the previous version.


```txr
@(next :args)
@{n-param}
@(next "top-rank-per-group.dat")
Employee Name,Employee ID,Salary,Department
@(collect :vars (record))
@name,@id,@salary,@dept
@(bind record (@(int-str salary) dept name id))
@(end)
@(do
  (let* ((n (int-str n-param))
         (dept-hash [group-by second record :equal-based])
         (ranked (collect-each ((rec (hash-values dept-hash)))
                   [[sort rec > first] 0..n])))
    (each ((d (hash-keys dept-hash))
           (dept-recs ranked))
      (put-line `Department: @d`)
      (each ((r dept-recs))
        (put-line `  @{r[2] 15} (@{r[3]})  $@{r[0] -6}`)))))
```



## Ursala

The algorithm used by the top function is to lex the data into fields, partition by the last field,
sort each partition descending by the second to last, take the first
n strings in each partition, and display the list of them in a reasonably understandable form.

```Ursala
#import std
#import nat

data =

-[
Employee Name,Employee ID,Salary,Department
Tyler Bennett,E10297,32000,D101
John Rappl,E21437,47000,D050
George Woltman,E00127,53500,D101
Adam Smith,E63535,18000,D202
Claire Buckman,E39876,27800,D202
David McClellan,E04242,41500,D101
Rich Holcomb,E01234,49500,D202
Nathan Adams,E41298,21900,D050
Richard Potter,E43128,15900,D101
David Motsinger,E27002,19250,D202
Tim Sampair,E03033,27000,D101
Kim Arlich,E10001,57000,D190
Timothy Grove,E16398,29900,D190]-

top "n" = @tt sep`,*; mat0+ ^C(~&hz,mat`,*yS)*+ take/*"n"+ *zK2 (nleq+ %np~~)-<x&yzNC

#show+

main = top3 data
```
<pre style="height:30ex;overflow:scroll">D190
Kim Arlich,E10001,57000
Timothy Grove,E16398,29900

D101
George Woltman,E00127,53500
David McClellan,E04242,41500
Tyler Bennett,E10297,32000

D202
Rich Holcomb,E01234,49500
Claire Buckman,E39876,27800
David Motsinger,E27002,19250

D050
John Rappl,E21437,47000
Nathan Adams,E41298,21900
```



## VBA

Creating an Excel pivot table to display the top n salaried employees per department. Data is read from a file.

```vb
Private Sub top_rank(filename As String, n As Integer)
    Workbooks.OpenText filename:=filename, Comma:=True
    Dim ws As Worksheet
    Set ws = Sheets.Add: ws.Name = "output"
    ActiveWorkbook.PivotCaches.Create(SourceType:=xlDatabase, SourceData:= _
        "data!R1C1:R14C4", Version:=6).CreatePivotTable TableDestination:= _
        "output!R3C1", TableName:="TableName", DefaultVersion:=6
    With Sheets("output").PivotTables("TableName")
        .InGridDropZones = True
        .RowAxisLayout xlTabularRow
        .AddDataField Sheets("output").PivotTables("TableName"). _
                PivotFields("Salary"), "Top rank", xlSum
        .PivotFields("Department").Orientation = xlRowField
        .PivotFields("Department").Position = 1
        .PivotFields("Salary").Orientation = xlRowField
        .PivotFields("Salary").Position = 2
        .PivotFields("Employee Name").Orientation = xlRowField
        .PivotFields("Employee Name").Position = 3
        .PivotFields("Employee ID").Orientation = xlRowField
        .PivotFields("Employee ID").Position = 4
        .PivotFields("Salary").PivotFilters.Add2 Type:=xlTopCount, _
            DataField:=Sheets("output").PivotTables("TableName"). _
            PivotFields("Top rank"), Value1:=n
        .PivotFields("Salary").Subtotals = Array(False, False, False, False, _
            False, False, False, False, False, False, False, False)
        .PivotFields("Employee Name").Subtotals = Array(False, False, False, _
            False, False, False, False, False, False, False, False, False)
        .PivotFields("Department").Subtotals = Array(False, False, False, False, _
            False, False, False, False, False, False, False, False)
        .ColumnGrand = False
        .PivotFields("Salary").AutoSort xlDescending, "Salary"
    End With
End Sub

Public Sub main()
    top_rank filename:="D:\data.txt", n:=3
End Sub
```


## zkl

This reads a data file, the contents of which are as given by the task description (minus the header). To protect privacy, identifying information has been stripped from the output (aka laziness enhanced coding).

```zkl
fcn setAppend(d,[(key,data)]){ d[key]=d.find(key,T).append(data) } //-->(key,(data,data...))

fcn topNsalaries(n){
   File("data.txt").pump(setAppend.fp(data:=D()),fcn(line){ //-->Dictionary(dept:salaries)
      line=line.strip().split(",");
      T(line[-1],line[-2]); //-->(dept,salary)
   });
   dss:=data.pump(List,fcn([(dept,ss)],N){ //-->(dept,(salaries), dept...)
      T(dept).append(ss.sort()[-N,*].reverse());
   }.fp1(n)).sort(fcn(a,b){a[0]<b[0]});
   foreach d,ss in (dss){
      "%s: %s".fmt(d,ss.concat(",")).println();
   }
}(3);
```

```txt

D050: 47000,21900
D101: 53500,41500,32000
D190: 57000,29900
D202: 49500,27800,19250

```

