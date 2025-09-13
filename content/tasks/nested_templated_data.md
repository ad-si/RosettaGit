+++
title = "Nested templated data"
description = ""
date = 2019-03-13T08:49:10Z
aliases = []
[extra]
id = 21855
[taxonomies]
categories = ["task"]
tags = []
+++

A template for data is an arbitrarily nested tree of integer indices.
    
Data payloads are given as a separate mapping, array or other simpler, flat,
association of indices to individual items of data, and are strings.

The idea is to create a data structure with the templates' nesting, and the 
payload corresponding to each index appearing at the position of each index.

Answers using simple string replacement or regexp are to be avoided. The idea is 
to use the native, or usual implementation of lists/tuples etc of the language
and to hierarchically traverse the template to generate the output.

## Task
Given the following input template `t` and list of payloads `p`:
```txt
# Square brackets are used here to denote nesting but may be changed for other,
# clear, visual representations of nested data appropriate to ones programming 
# language.
t = [
    [[1, 2],
     [3, 4, 1], 
     5]]

p = 'Payload#0' ... 'Payload#6'
```


The correct output should have the following structure, (although spacing and 
linefeeds may differ, the nesting and order should follow):
```txt
[[['Payload#1', 'Payload#2'],
  ['Payload#3', 'Payload#4', 'Payload#1'],
  'Payload#5']]
```


1. Generate the output for the above template, <code>t</code>.


Optional extended tasks:
2. Show which payloads remain unused.

3. Give some indication/handling of indices without a payload.

Show output on this page.

## Bracmat

The uninstantiated template and the instantiated template are JSON structures. The payloads are listed in a Bracmat list. The <code>get</code> function is instructed to read input from memory and to parse it as JSON. The output of this call is a Bracmat structure (not shown) that is assigned to <code>template</code>. The <code>instantiate</code> function recursively traverses the template's tree structure. If the current node is a number, then that number is used as the key into the payloads list. The corresponding payload is then returned. If the current node is not a number, then it is assumed that the current node is a binary (sub)tree. The left hand side (called <code>a</code>) and the right hand side (called <code>b</code>) are instantiated and their combination is returned. Finally the instantiated tree is transformed back to JSON format and output.

```bracmat
(   get$("[
    [[1, 2],
     [3, 4, 1],
     5]]",MEM,JSN)
  : ?template
&     (0.payload#0)
      (1.payload#1)
      (2.payload#2)
      (3.payload#3)
      (4.payload#4)
      (5.payload#5)
      (6.payload#6)
  : ?payloads
& ( instantiate
  =   tmplt plds pld a b
    .   !arg:(?tmplt.?plds)
      & (   !tmplt:#
          & !plds:? (!tmplt.?pld) ?
          & !pld
        |   !tmplt:%?a_%?b
          & (instantiate$(!a.!plds))_(instantiate$(!b.!plds))
        )
  )
& out$(jsn$(instantiate$(!template.!payloads)))
);
```

{{out}}

```txt
[[[payload#1,payload#2],[payload#3,payload#4,payload#1],payload#5]]
```



## Crystal


```Ruby
def with_payload(template, payload, used = nil)
    template.map do |item|
      if item.is_a? Enumerable
        with_payload(item, payload, used)
      else
        used << item
        payload[item]
      end
    end
end

p = {"Payload#0", "Payload#1", "Payload#2", "Payload#3", "Payload#4", "Payload#5", "Payload#6"}
t = { { {1, 2}, {3, 4, 1}, 5}}
used = Set(Int32).new
puts with_payload(t, p, used)

unused = Set(Int32).new((0..6).to_a) - used
puts "Unused indices: #{unused}"
```


{{out}}

```txt
{{{"Payload#1", "Payload#2"}, {"Payload#3", "Payload#4", "Payload#1"}, "Payload#5"}}
Unused indices: Set{0, 6}
```



## Factor

Words for traversing nested sequences can be found in the <code>sequences.deep</code> vocabulary. Factor's prettyprinter attempts to print structures on a single line (64 characters by default, though this can be changed) if they will fit. Otherwise, the prettyprinter will break them up into multiple lines, preferring to show one member per line if possible. <code>f</code>, Factor's false/nil value, is used to indicate a missing payload.

```factor
USING: formatting kernel literals math sequences sequences.deep ;
IN: rosetta-code.nested-template-data

CONSTANT: payloads $[ 7 <iota> [ "Payload#%d" sprintf ] map ]

: insert-payloads ( template -- data-structure )
    [ dup fixnum? [ payloads ?nth ] when ] deep-map ;
    
{ { { 1 2 }
    { 3 4 1 }
    5 } }
    
{ { { 1 2 }
    { 10 4 1 }
    5 } }
    
[ dup insert-payloads "Template: %u\nData Structure: %u\n"
printf ] bi@
```

{{out}}

```txt

Template: { { { 1 2 } { 3 4 1 } 5 } }
Data Structure: {
    {
        { "Payload#1" "Payload#2" }
        { "Payload#3" "Payload#4" "Payload#1" }
        "Payload#5"
    }
}
Template: { { { 1 2 } { 10 4 1 } 5 } }
Data Structure: {
    {
        { "Payload#1" "Payload#2" }
        { f "Payload#4" "Payload#1" }
        "Payload#5"
    }
}

```



## Go

Go's standard library includes a "text/template" package which can be used for this task.


The integer indices are represented by the keys of a map whose corresponding value is the appropriate payload string. Templates have their own mini-language and, for a map P with key n, the expression: 
```html
{{index .P n}}
```
 will be replaced by the corresponding payload. 


If an integer index either doesn't exist in the map or maps to an empty payload, then the above expression will simply be replaced by an empty string when the template is executed.  

```go
package main

import (
    "fmt"
    "os"
    "sort"
    "strings"
    "text/template"
)

func main() {
    const t = `[[[{{index .P 1}}, {{index .P 2}}],
  [{{index .P 3}}, {{index .P 4}}, {{index .P 1}}], 
  {{index .P 5}}]]
`
    type S struct {
        P map[int]string
    }

    var s S
    s.P = map[int]string{
        0: "'Payload#0'", 1: "'Payload#1'", 2: "'Payload#2'", 3: "'Payload#3'",
        4: "'Payload#4'", 5: "'Payload#5'", 6: "'Payload#6'",
    }
    tmpl := template.Must(template.New("").Parse(t))
    tmpl.Execute(os.Stdout, s)

    var unused []int
    for k, _ := range s.P {
        if !strings.Contains(t, fmt.Sprintf("{{index .P %d}}", k)) {
            unused = append(unused, k)
        }
    }
    sort.Ints(unused)
    fmt.Println("\nThe unused payloads have indices of :", unused)
}
```


{{out}}

```txt

[[['Payload#1', 'Payload#2'],
  ['Payload#3', 'Payload#4', 'Payload#1'], 
  'Payload#5']]

The unused payloads have indices of : [0 6]

```




## Julia

The array structure needs to be specified as Any type of data, to allow later assignment of strings to the paces in the array where there are integers.

```julia
t = ([Any[Any[1, 2],
          Any[3, 4, 1],
          5]])

p = ["Payload#$x" for x in 0:6]

for (i, e) in enumerate(t)
    if e isa Number
        t[i] = p[e + 1]
    else
        for (j, f) in enumerate(e)
            if f isa Number
                e[j] = p[f + 1]
            else
                for (k, g) in enumerate(f)
                    if g isa Number
                        f[k] = p[g + 1]
                    end
                end
            end
        end
    end
end


show(t)

```
{{output}}
```txt

Array{Any,1}[[Any["Payload#1", "Payload#2"], Any["Payload#3", "Payload#4", "Payload#1"], "Payload#5"]]

```



## M2000 Interpreter



```M2000 Interpreter

Font "Courier New"
cls
Module Checkit {
      t=(((1,2), (3,4,1),5),)    ' use (1,) for one item tuple
      Tuple$ = lambda$ (a, feed$=" ") -> {
            \\ we can pass a tuple of two arguments or two arguments
            k=each(a)
            lim=len(a)-1
            res$="("
            link a to a() ' to use type$()
            while k {
                  if type$(a(k^))="mArray" then
                        res$+=Lambda$(array(a, k^),feed$+" ")
                        if k^<lim then
                              res$+={,
                              }+feed$
                        end if
                  else      
                        res$+= trim$(str$(array(k)))
                        if k^<lim then res$+=", "
                  end if
            }
            =res$+")"
      }
      TotalPayload = lambda (a)->{
            k=each(a)
            link a to a()
            res=0
            while k {
                  if type$(a(k^))="mArray" then
                        res+=Lambda(a(k^))
                  else
                        res++
                  end if
            }
            =res
      }
      Payload = lambda (a,payloads as list)->{
           misspayloads=List
           used=list
           inner=lambda misspayloads, p=1, payloads, used (a)-> {
                  k=each(a)
                  res=(,)
                  link a to a()
                  while k {
                        if type$(a(k^))="mArray" then
                             Append res, (Lambda(a(k^)),)
                        else
                              curpayload$="Payload#"+trim$(str$(array(k)))

                              if not exist(payloads, curpayload$) Then
                                    if not exist(used, curpayload$) Then
                                          Append res, ("missing#pos"+trim$(str$(p)),)
                                          append misspayloads, p:=curpayload$
                                    else                               
                                          Append res, (curpayload$,)
                                    End if
                                    p++
                              else
                                    Append res, (curpayload$,)
                                    if exist(payloads, curpayload$) then 
                                          delete payloads, curpayload$
                                          if not exist(used, curpayload$) then append used, curpayload$
                                    end if
                                    p++
                              end if
                        end if
                  }
                  =res
            }
            =inner(a), payloads, misspayloads
      }
      Expand$ =lambda$ (a as array, unused as list, misspayloads as list)-> {
            Read ? space$
            inner$= lambda$ (a, feed$=" ")->{
                  k=each(a)
                  lim=len(a)-1
                  res$="["
                  link a to a() ' to use type$()
                  while k {
                        if type$(a(k^))="mArray" then
                              res$+=Lambda$(array(a, k^),feed$+" ")
                              if k^<lim then
                                    res$+={,
                                    }+feed$
                              end if
                        else      
                              res$+= "'"+array$(k)+"'"
                              if k^<lim then res$+=", "
                        end if
                  }
                  =res$+"]"      
            }
            document unused$="Unused Payloads"+{
            }
            if len(unused)>0 then
                  un=each(unused)
                  while un {
                        unused$="  "+eval$(un)+{
                        }
                  }
            else
                  unused$="  -"
            end if
            if len(misspayloads)>0 then
                  un=each(misspayloads)
                  lim=len(misspayloads)-1
                  document missing$="Missing in position: "+{
                  }
                  while un {
                        missing$="  "+eval$(un)+"-pos"+eval$(un,un^)+{
                        }
                  }
                  =inner$(a, space$)+{
                  }+unused$+missing$
            Else
                  =inner$(a, space$)+{
                  } + unused$
            End if
      }
      flush
      Data t, (((1,10), (3,4,16),5),)
      While not empty {
            Read t
            Document result$="Payloads:"+{
            }
            p=list
            for i=0 to 6 {
                  Append p, "Payload#"+trim$(str$(i))
                  result$="  "+eval$(p, i)+{
                  }
            }
            result$="Template:"+{
            }
            result$="  "+Tuple$(t, "   ")+{
            }
            result$="Template with Payloads:"+{
            }
            m=Payload(t, p)
            result$="  "+Expand$(!m, "   ")
            
            clipboard result$
      }
      
}
Checkit
Report clipboard$

```

{{out}}
<pre style="height:30ex;overflow:scroll">
Payloads:
  Payload#0
  Payload#1
  Payload#2
  Payload#3
  Payload#4
  Payload#5
  Payload#6
Template:
  (((1, 2),
    (3, 4, 1),
    5))
Template with Payloads:
  [[['Payload#1', 'Payload#2'],
    ['Payload#3', 'Payload#4', 'Payload#1'],
    'Payload#5']]
Unused Payloads
  Payload#0
  Payload#6
Payloads:
  Payload#0
  Payload#1
  Payload#2
  Payload#3
  Payload#4
  Payload#5
  Payload#6
Template:
  (((1, 10),
    (3, 4, 16),
    5))
Template with Payloads:
  [[['Payload#1', 'missing#pos2'],
    ['Payload#3', 'Payload#4', 'missing#pos5'],
    'Payload#5']]
Unused Payloads
  Payload#0
  Payload#6
  Payload#2
Missing in position: 
  Payload#10-pos2
  Payload#16-pos5
</pre >


## Perl

Only handles nesting one level deep. Missing data is <code>undef</code> in the data structure, an empty string in the pretty-printer.

```perl
sub fulfill {
    my   @payloads;
    push @payloads, 'Payload#' . $_ for 0..5;
    my      @result;
    push    @result, ref $_ eq 'ARRAY' ? [@payloads[@$_]] : @payloads[$_] for @{@_[0]};
    return [@result];
}

sub formatted {
    my $result;
    $result .= ref $_ eq 'ARRAY' ? '[ "'. join('", "', @$_) . '" ], ' : qq{"$_"} for @{@_[0]};
    return '[ ' . $result . " ]\n";
}

print formatted fulfill( [[1,2], [ 3,4,1], 5] );
print formatted fulfill( [[1,2], [10,4,1], 5] );

```

{{out}}

```txt
[ [ "Payload#1", "Payload#2" ], [ "Payload#3", "Payload#4", "Payload#1" ], "Payload#5" ]
[ [ "Payload#1", "Payload#2" ], [ "", "Payload#4", "Payload#1" ], "Payload#5" ]
```



## Perl 6

{{works with|Rakudo|2018.04.01}}
Explicitly not using strings, using one data structure to fill in another. Since it ''isn't'' a string, the output format removes the newlines from the template; line feed (white space in general) isn't particularly significant in Perl 6 data structures. It does preserve the nesting though. In the second example, payload "buckets" that don't exist result in an undefined value being inserted; by default: Any. 

```perl6
say join "\n  ", '##PAYLOADS:', |my @payloads = 'Payload#' X~ ^7;

for [
     (((1, 2),
       (3, 4, 1),
       5),),

     (((1, 2),
       (10, 4, 1),
       5),)
    ] {
    say "\n      Template: ", $_.perl;
    say "Data structure: { @payloads[|$_].perl }";
}
```

{{out}}

```txt
##PAYLOADS:
  Payload#0
  Payload#1
  Payload#2
  Payload#3
  Payload#4
  Payload#5
  Payload#6

      Template: $(((1, 2), (3, 4, 1), 5),)
Data structure: ((("Payload#1", "Payload#2"), ("Payload#3", "Payload#4", "Payload#1"), "Payload#5"),)

      Template: $(((1, 2), (10, 4, 1), 5),)
Data structure: ((("Payload#1", "Payload#2"), (Any, "Payload#4", "Payload#1"), "Payload#5"),)
```



## Phix

This task almost feels custom-built for Phix.

Note that Phix indexes are normally 1-based, but to better match the task description those in the templates are 0-based

```Phix
constant template = { { { 1, 2 }, { 3, 4, 1, }, 5 } },
         template2 =  { { { 1, 2 }, { 10, 4, 1 }, 5 } },
         payload = {"Payload#0", "Payload#1", "Payload#2", "Payload#3", "Payload#4", "Payload#5", "Payload#6"}
sequence unused = repeat(true,length(payload)),
         missing = {}

function fill(object t, sequence p)
    if integer(t) then
        if t>=length(p) then
            if not find(t,missing) then missing &= t end if
            return sprintf("*** index error (%d>%d) ***",{t,length(p)-1})
        end if
        unused[t+1] = false
        return p[t+1]
    end if
    for i=1 to length(t) do
        t[i] = fill(t[i],p)
    end for
    return t
end function

ppOpt({pp_Nest,2})
pp(fill(template,payload))
pp(fill(template2,payload))

sequence idx = {}
for i=1 to length(unused) do
    if unused[i] then idx &= i-1 end if
end for
printf(1,"\nThe unused payloads have indices of :%s\n", {sprint(idx)})

if length(missing) then
    printf(1,"Missing payloads: %s\n", {sprint(missing)})
end if
```


```txt

{{{"Payload#1", "Payload#2"},
  {"Payload#3", "Payload#4", "Payload#1"},
  "Payload#5"}}
{{{"Payload#1", "Payload#2"},
  {"*** index error (10>6) ***", "Payload#4", "Payload#1"},
  "Payload#5"}}

The unused payloads have indices of :{0,6}
Missing payloads: {10}

```



## Python

This uses f-strings from Python3.6+.

I choose to use nested tuples for the template structure, and a dict to map integer indices to corresponding payload strings.

A distinctive string is used to indicate missing payloads.

```python
from pprint import pprint as pp

class Template():
    def __init__(self, structure):
        self.structure = structure
        self.used_payloads, self.missed_payloads = [], []
    
    def inject_payload(self, id2data):
        
        def _inject_payload(substruct, i2d, used, missed):
            used.extend(i2d[x] for x in substruct if type(x) is not tuple and x in i2d)
            missed.extend(f'??#{x}' 
                          for x in substruct if type(x) is not tuple and x not in i2d)
            return tuple(_inject_payload(x, i2d, used, missed) 
                           if type(x) is tuple 
                           else i2d.get(x, f'??#{x}') 
                         for x in substruct)
                           
        ans = _inject_payload(self.structure, id2data, 
                              self.used_payloads, self.missed_payloads)
        self.unused_payloads = sorted(set(id2data.values()) 
                                      - set(self.used_payloads))
        self.missed_payloads = sorted(set(self.missed_payloads))
        return ans

if __name__ == '__main__':
    index2data = {p: f'Payload#{p}' for p in range(7)}
    print("##PAYLOADS:\n  ", end='')
    print('\n  '.join(list(index2data.values())))
    for structure in [
     (((1, 2),
       (3, 4, 1),
       5),),
    
     (((1, 2),
       (10, 4, 1),
       5),)]:
        print("\n\n# TEMPLATE:")
        pp(structure, width=13)
        print("\n TEMPLATE WITH PAYLOADS:")
        t = Template(structure)
        out = t.inject_payload(index2data)
        pp(out)
        print("\n UNUSED PAYLOADS:\n  ", end='')
        unused = t.unused_payloads
        print('\n  '.join(unused) if unused else '-')
        print(" MISSING PAYLOADS:\n  ", end='')
        missed = t.missed_payloads
        print('\n  '.join(missed) if missed else '-')
```


{{out}}

```txt
##PAYLOADS:
  Payload#0
  Payload#1
  Payload#2
  Payload#3
  Payload#4
  Payload#5
  Payload#6


# TEMPLATE:
(((1, 2),
  (3, 4, 1),
  5),)

 TEMPLATE WITH PAYLOADS:
((('Payload#1', 'Payload#2'),
  ('Payload#3', 'Payload#4', 'Payload#1'),
  'Payload#5'),)

 UNUSED PAYLOADS:
  Payload#0
  Payload#6
 MISSING PAYLOADS:
  -


# TEMPLATE:
(((1, 2),
  (10, 4, 1),
  5),)

 TEMPLATE WITH PAYLOADS:
((('Payload#1', 'Payload#2'),
  ('??#10', 'Payload#4', 'Payload#1'),
  'Payload#5'),)

 UNUSED PAYLOADS:
  Payload#0
  Payload#3
  Payload#6
 MISSING PAYLOADS:
  ??#10
```



## Racket


<code>rackunit</code> is used to test the outcomes of template application. So no output indicates expectations met (i.e. success).


```racket
#lang racket

(define current-not-found-handler
  (make-parameter (λ (idx max) (raise-range-error 'substitute-template "integer?" "" idx 0 max))))

(define ((substitute-template template) payloads)
  (define match-function
    (match-lambda
      [(? nonnegative-integer? idx) #:when (< idx (length payloads)) (list-ref payloads idx)]
      [(? nonnegative-integer? idx) ((current-not-found-handler) idx (sub1 (length payloads)))]
      [(list (app match-function substitutions) ...) substitutions]))
  (match-function template))

(module+ test
  (require rackunit)

  (define substitute-in-t (substitute-template '(((1 2)
                                                  (3 4 1)
                                                  5))))
 
  (define p '(Payload#0 Payload#1 Payload#2 Payload#3 Payload#4 Payload#5 Payload#6))

  (check-equal? (substitute-in-t p)
                '(((Payload#1 Payload#2)
                   (Payload#3 Payload#4 Payload#1)
                   Payload#5)))

  (define out-of-bounds-generating-template-substitution (substitute-template '(7)))
  
  (check-exn exn:fail:contract? (λ () (out-of-bounds-generating-template-substitution p)))

  (parameterize ((current-not-found-handler (λ (idx max) (format "?~a" idx))))
    (check-equal? (out-of-bounds-generating-template-substitution p) '("?7"))))
```



## REXX


### version 1


```rexx
/* REXX */
tok.=''
Do i=0 To 6
  tok.i="'Payload#"i"'"
  End
t1='[[[1,2],[3,4,1],5]]'
t2='[[[1,6],[3,4,7,0],5]]'
Call transform t1
Call transform t2
Exit

transform:
Parse Arg t 1 tt
/* http://rosettacode.org/wiki/Nested_templated_data */
/*
[[['Payload#1', 'Payload#2'],
  ['Payload#3', 'Payload#4', 'Payload#1'],
  'Payload#5']]
*/
lvl=0
n.=0
o=''
w=''
used.=0
Do While t<>''
  Parse Var t c +1 1 c3 +3 1 c2 +2
  u=' '
  v=' '
  Select
    When c3='],[' Then Do
      o=o'  '
      w=w'  '
      t=substr(t,3)
      End
    When c2='],' Then Do
      o=o' '
      w=w' '
      t=substr(t,2)
      lvl=lvl-1
      End
    When c='[' Then
      lvl=lvl+1
    When c=']' Then
      lvl=lvl-1
    When c=',' Then
      Nop
    Otherwise Do
      u=lvl
      v=c
      End
    End
  t=substr(t,2)
  o=o||u
  w=w||v
  End
Say 'Template' tt
Do i=1 By 1 While w<>''
  If i=1 Then Do
    w=substr(w,4)
    p=pos('  ',w)
    Call o '[[['cont(left(w,p-1))'],'
    w=substr(w,p)
    End
  Else Do
    If left(w,3)='' Then Do
      w=substr(w,4)
      p=pos('  ',w)
      Call o '  ['cont(left(w,p-1))'],'
      w=substr(w,p)
      End
    Else Do
      w=substr(w,3)
      p=pos('  ',w)
      Call o '  'cont(left(w,p-1))']]'
      w=substr(w,p)
      End
    End
  End
Do i=0 To 6
  If used.i=0 Then Say 'Payload' i 'not used'
  End
Call o ' '
Return

o: Say arg(1)
   Return

cont: Procedure Expose tok. used.
  Parse Arg list
  res=''
  Do while list>''
    Parse Var list i list
    res= res tok(i)','
    End
  res=strip(res)
  res=strip(res,'T',',')
  Return res

tok: Procedure Expose tok. used.
Parse Arg i
If tok.i<>'' Then Do
  used.i=1
  Return tok.i
  End
Else
  Return "'Payload#" i "not defined'"
```

{{out}}

```txt
Template [[[1,2],[3,4,1],5]]
[[['Payload#1', 'Payload#2'],
  ['Payload#3', 'Payload#4', 'Payload#1'],
  'Payload#5']]
Payload 0 not used
Payload 6 not used

Template [[[1,6],[3,4,7,0],5]]
[[['Payload#1', 'Payload#6'],
  ['Payload#3', 'Payload#4', 'Payload# 7 not defined', 'Payload#0'],
  'Payload#5']]
Payload 2 not used
```



### version 2


```rexx
/* REXX */
tok.=''
Do i=0 To 6
  tok.i="'Payload#"i"'"
  End
t1='[[[1,2],[ 3,4,1],5]]'
t2='1[[[1,6]],[[3,4[7] 0],5]3]9 [8] 9'
Call transform t1
Call transform t2
Exit

transform:
Parse Arg t 1 tt
t=space(t,0)
lvl=0
t.=0
used.=0
undefined=''
Do While t<>''
  Parse Var t c +1 t
  Select
    When c='[' Then
      lvl=lvl+1
    When c=']' Then
      lvl=lvl-1
    When c=',' Then
      Nop
    Otherwise Do
      t=c||t
      p1=pos(']',t)
      p2=pos('[',t)
      Select
        When p2=0 Then p=p1
        When p1=0 Then p=p2
        Otherwise p=min(p1,p2)
        End
      If p=0 Then Do
        Call mem lvl': >'t'<'
        t=''
        End
      Else Do
        Call mem lvl': >'left(t,p-1)'<'
        t=substr(t,p)
        End
      End
    End
  End
Call show
Return

mem:
z=t.0+1
t.z=arg(1)
t.0=z
Return

show:
Say tt
Say 'lvl Element'
Do i=1 To t.0
  Parse Var t.i lvl ':' '>' cont '<'
  ol=right(lvl,3) copies(' ',lvl*3)cont(cont)
  Say ol
  End
Do i=0 To 6
  If used.i=0 Then Say 'Payload' i 'not used'
  End
Do While undefined>''
  Parse Var undefined i undefined
  Say 'Payload' i 'is not defined'
  End
Call o ' '
Return

cont: Procedure Expose tok. used. undefined
  Parse Arg list
  list=translate(list,' ',',')
  res=''
  Do while list>''
    Parse Var list i list
    res= res tok(i)','
    End
  res=strip(res)
  res=strip(res,'T',',')
  Return res

tok: Procedure Expose tok. used. undefined
Parse Arg i
If tok.i<>'' Then Do
  used.i=1
  Return tok.i
  End
Else Do
  If wordpos(i,undefined)=0 Then
    undefined=undefined i
  Return "'Payload#"i "not defined'"
  End

o: Say arg(1)
   Return
```

{{out}}

```txt
[[[1,2],[ 3,4,1],5]]
lvl Element
  3          'Payload#1', 'Payload#2'
  3          'Payload#3', 'Payload#4', 'Payload#1'
  2       'Payload#5'
Payload 0 not used
Payload 6 not used

1[[[1,6]],[[3,4[7] 0],5]3]9 [8] 9
lvl Element
  0 'Payload#1'
  3          'Payload#1', 'Payload#6'
  3          'Payload#3', 'Payload#4'
  4             'Payload#7 not defined'
  3          'Payload#0'
  2       'Payload#5'
  1    'Payload#3'
  0 'Payload#9 not defined'
  1    'Payload#8 not defined'
  0 'Payload#9 not defined'
Payload 2 not used
Payload 7 is not defined
Payload 9 is not defined
Payload 8 is not defined
```



## VBA

VBA allows arrays of variant, so the elements of an array can be both scalars as arrays of different sizes.

```vb
Public Sub test()
    Dim t(2) As Variant
    t(0) = [{1,2}]
    t(1) = [{3,4,1}]
    t(2) = 5
    p = [{"Payload#0","Payload#1","Payload#2","Payload#3","Payload#4","Payload#5","Payload#6"}]
    Dim q(6) As Boolean
    For i = LBound(t) To UBound(t)
        If IsArray(t(i)) Then
            For j = LBound(t(i)) To UBound(t(i))
                q(t(i)(j)) = True
                t(i)(j) = p(t(i)(j) + 1)
            Next j
        Else
            q(t(i)) = True
            t(i) = p(t(i) + 1)
        End If
    Next i
    For i = LBound(t) To UBound(t)
        If IsArray(t(i)) Then
            Debug.Print Join(t(i), ", ")
        Else
            Debug.Print t(i)
        End If
    Next i
    For i = LBound(q) To UBound(q)
        If Not q(i) Then Debug.Print p(i + 1); " is not used"
    Next i
End Sub
```
{{out}}

```txt
Payload#1, Payload#2
Payload#3, Payload#4, Payload#1
Payload#5
Payload#0 is not used
Payload#6 is not used
```


## zkl

Formatting is lost as zkl is format free. A pretty printer could be written but
the tasks asks for a data structure.

Void is used as a marker for an unknown payload.

```zkl
var payloads=[1..6].pump(List,"Payload#".append);

fcn get(n){ try{ payloads[n - 1] }catch{ Void } }
fcn sub(list){ list.pump(List, fcn(n){ if(n.isType(List)) sub(n) else get(n) }) }
```


```zkl
foreach p in (T( 
     T(T(T(1, 2),
         T(3, 4, 1),
         5),),
   T(T(T(1, 2),
       T(10,4, 1),
         5),))){
   println("      Template: %s\nData structure: %s".fmt(p,sub(p)));
}
```

{{out}}

```txt

      Template: L(L(L(1,2),L(3,4,1),5))
Data structure: L(L(L("Payload#1","Payload#2"),L("Payload#3","Payload#4","Payload#1"),"Payload#5"))
      Template: L(L(L(1,2),L(10,4,1),5))
Data structure: L(L(L("Payload#1","Payload#2"),L(Void,"Payload#4","Payload#1"),"Payload#5"))

```
