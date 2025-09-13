+++
title = "Process SMIL directives in XML data"
description = ""
date = 2019-08-01T14:28:20Z
aliases = []
[extra]
id = 5265
[taxonomies]
categories = ["task"]
tags = []
+++

In order to represent evolutions of contents over time, the [http://www.w3.org/TR/SMIL/ SMIL] standard provides a solution to record the animation of data. Smil animations can be added to any kind of contents formated in XML.

* [[wp:Synchronized_Multimedia_Integration_Language|SMIL on Wikipedia]] and [http://www.w3.org/TR/SMIL/smil-animation.html#q35 at W3]

The task is to create an utility that given the first Smiled XML file, would return the following ones:


```xml
<?xml version="1.0" ?>
<smil>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color='1 1 1' location='0 2 0'/>
    <Shape>
      <Box size='2 1 2'>
        <animate attributeName="size" from="2 1 2"
                                        to="1 2 1" begin="0s" dur="10s"/>
      </Box>
      <Appearance>
        <Material diffuseColor='0.0 0.6 1.0'>
          <animate attributeName="diffuseColor" from="0.0 0.6 1.0"
                                                  to="1.0 0.4 0.0" begin="0s" dur="10s"/>
        </Material>
      </Appearance>
    </Shape>
  </Scene>
</X3D>
</smil>
```


At t = 0 second here is the expected output:


```xml
<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color='1 1 1' location='0 2 0'/>
    <Shape>
      <Box size='2 1 2'/>
      <Appearance>
        <Material diffuseColor='0.0 0.6 1.0'/>
      </Appearance>
    </Shape>
  </Scene>
</X3D>
```


At t = 2 second here is the expected output:


```xml
<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color='1 1 1' location='0 2 0'/>
    <Shape>
      <Box size='1.8 1.2 1.8'/>
      <Appearance>
        <Material diffuseColor='0.2 0.56 0.8'/>
      </Appearance>
    </Shape>
  </Scene>
</X3D>
```



## Go

```go
package main

import (
    "fmt"
    "github.com/beevik/etree"
    "log"
    "os"
    "strconv"
    "strings"
)

type animData struct {
    element *etree.Element
    attrib  string
    from    string
    to      string
    begin   float64
    dur     float64
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func (ad *animData) AtTime(t float64) string {
    beg := ad.begin
    end := beg + ad.dur
    if t < beg || t > end {
        log.Fatalf("time must be in interval [%g, %g]", beg, end)
    }
    fromSplit := strings.Fields(ad.from)
    toSplit := strings.Fields(ad.to)
    le := len(fromSplit)
    interSplit := make([]string, le)
    for i := 0; i < le; i++ {
        fromF, err := strconv.ParseFloat(fromSplit[i], 64)
        check(err)
        toF, err := strconv.ParseFloat(toSplit[i], 64)
        check(err)
        interF := (fromF*(end-t) + toF*(t-beg)) / ad.dur
        interSplit[i] = fmt.Sprintf("%.2f", interF)
    }
    return strings.Join(interSplit, " ")
}

func main() {
    doc := etree.NewDocument()
    check(doc.ReadFromFile("smil.xml"))
    smil := doc.SelectElement("smil")
    if smil == nil {
        log.Fatal("'smil' element not found")
    }
    x3d := smil.SelectElement("X3D")
    if x3d == nil {
        log.Fatal("'X3D' element not found")
    }
    doc.SetRoot(x3d) // remove 'smil' element
    var ads []*animData
    for _, a := range doc.FindElements("//animate") {
        attrib := a.SelectAttrValue("attributeName", "?")
        from := a.SelectAttrValue("from", "?")
        to := a.SelectAttrValue("to", "?")
        beginS := a.SelectAttrValue("begin", "?")
        durS := a.SelectAttrValue("dur", "?")
        if attrib == "?" || from == "?" || to == "?" ||
            beginS == "?" || durS == "?" {
            log.Fatal("an animate element has missing attribute(s)")
        }
        begin, err := strconv.ParseFloat(beginS[:len(beginS)-1], 64)
        check(err)
        dur, err := strconv.ParseFloat(durS[:len(durS)-1], 64)
        check(err)
        p := a.Parent()
        if p == nil {
            log.Fatal("an animate element has no parent")
        }
        pattrib := p.SelectAttrValue(attrib, "?")
        if pattrib == "?" {
            log.Fatal("an animate element's parent has missing attribute")
        }
        ads = append(ads, &animData{p, attrib, from, to, begin, dur})
        p.RemoveChild(a) // remove 'animate' element
    }
    ts := []float64{0, 2}
    for _, t := range ts {
        for _, ad := range ads {
            s := ad.AtTime(t)
            ad.element.CreateAttr(ad.attrib, s)
        }
        doc.Indent(2)
        fmt.Printf("At time = %g seconds:\n\n", t)
        doc.WriteTo(os.Stdout)
        fmt.Println()
    }
}
```


```txt

At time = 0 seconds:

<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color="1 1 1" location="0 2 0"/>
    <Shape>
      <Box size="2.00 1.00 2.00"/>
      <Appearance>
        <Material diffuseColor="0.00 0.60 1.00"/>
      </Appearance>
    </Shape>
  </Scene>
</X3D>

At time = 2 seconds:

<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color="1 1 1" location="0 2 0"/>
    <Shape>
      <Box size="1.80 1.20 1.80"/>
      <Appearance>
        <Material diffuseColor="0.20 0.56 0.80"/>
      </Appearance>
    </Shape>
  </Scene>
</X3D>

```



## Phix


```Phix
include builtins\xml.e

constant xml = """
<?xml version="1.0" ?>
<smil>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color='1 1 1' location='0 2 0'/>
    <Shape>
      <Box size='2 1 2'>
        <animate attributeName="size" from="2 1 2"
                                        to="1 2 1" begin="0s" dur="10s"/>
      </Box>
      <Appearance>
        <Material diffuseColor='0.0 0.6 1.0'>
          <animate attributeName="diffuseColor" from="0.0 0.6 1.0"
                                                  to="1.0 0.4 0.0" begin="0s" dur="10s"/>
        </Material>
      </Appearance>
    </Shape>
  </Scene>
</X3D>
</smil>
"""

function scan_all(sequence s, fmt)
    for i=1 to length(s) do
        {{s[i]}} = scanf(s[i],fmt)
    end for
    return s
end function

function animate_contents(sequence doc, atom t)
    sequence a = xml_get_nodes(doc,"animate")
    if a={} then
        for i=1 to length(doc[XML_CONTENTS]) do
            doc[XML_CONTENTS][i] = animate_contents(doc[XML_CONTENTS][i],t)
        end for
    else
        for i=1 to length(doc[XML_CONTENTS]) do
            if doc[XML_CONTENTS][i][XML_TAGNAME]="animate" then
                string name = xml_get_attribute(doc[XML_CONTENTS][i],"attributeName"),
                       vfrm = xml_get_attribute(doc[XML_CONTENTS][i],"from"),
                       v_to = xml_get_attribute(doc[XML_CONTENTS][i],"to"),
                       sbeg = xml_get_attribute(doc[XML_CONTENTS][i],"begin"),
                       sdur = xml_get_attribute(doc[XML_CONTENTS][i],"dur")
                sequence from = scan_all(split(vfrm),"%f"),
                         to_s = scan_all(split(v_to),"%f")
                atom {{begin}} = scanf(sbeg,"%fs"),
                     {{durat}} = scanf(sdur,"%fs"),
                     fj = begin+durat-t,
                     tj = t-begin
                -- plenty more error handling possible here...
                if tj<0 or fj<0 or length(from)!=length(to_s) then ?9/0 end if
                for j=1 to length(from) do
                    from[j] = sprintf("%.2f",(from[j]*fj+to_s[j]*tj)/durat)
                end for
                doc = xml_set_attribute(doc,name,join(from," "))
                doc[XML_CONTENTS][i..i] = "" -- remove 'animate'
                exit
            end if
        end for
    end if
    return doc
end function

function animate(sequence doc, atom t)
    doc[XML_CONTENTS] = doc[XML_CONTENTS][XML_CONTENTS][1]  -- remove smil
    doc[XML_CONTENTS] = animate_contents(doc[XML_CONTENTS],t)
    return doc
end function

sequence doc = xml_parse(xml)
if doc[XML_DOCUMENT]!="document"
or doc[XML_CONTENTS][XML_TAGNAME]!="smil"
or length(doc[XML_CONTENTS][XML_CONTENTS])!=1
or doc[XML_CONTENTS][XML_CONTENTS][1][XML_TAGNAME]!="X3D" then
    ?9/0
end if
printf(1,"At time = 0:\n\n")
puts(1,xml_sprint(animate(doc,0)))
printf(1,"\nAt time = 2:\n\n")
puts(1,xml_sprint(animate(doc,2)))
```

```txt

At time = 0:

<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0" />
    <PointLight color="1 1 1" location="0 2 0" />
    <Shape>
      <Box size="2.00 1.00 2.00" />
      <Appearance>
        <Material diffuseColor="0.00 0.60 1.00" />
      </Appearance>
    </Shape>
  </Scene>
</X3D>

At time = 2:

<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0" />
    <PointLight color="1 1 1" location="0 2 0" />
    <Shape>
      <Box size="1.80 1.20 1.80" />
      <Appearance>
        <Material diffuseColor="0.20 0.56 0.80" />
      </Appearance>
    </Shape>
  </Scene>
</X3D>

```



## Tcl

```tcl
package require Tcl 8.6
package require tdom

# Applies a time-based interpolation to generate a space-separated list
proc interpolate {time info} {
    dict with info {
	scan $begin "%fs" begin
	scan $dur "%fs" dur
    }
    if {$time < $begin} {
	return $from
    } elseif {$time > $begin+$dur} {
	return $to
    }
    set delta [expr {($time - $begin) / $dur}]
    return [lmap f $from t $to {expr {$f + ($t-$f)*$delta}}]
}

# Applies SMIL <transform> elements to their container
proc applySMILtransform {sourceDocument time} {
    set doc [dom parse [$sourceDocument asXML]]
    foreach smil [$doc selectNodes //smil] {
	foreach context [$smil selectNodes {//*[animate]}] {
	    set animator [$context selectNodes animate]
	    set animated [$context selectNodes @[$animator @attributeName]]
	    $context removeChild $animator
	    $context setAttribute [$animator @attributeName] \
		[interpolate $time [lindex [$animator asList] 1]]
	}
	if {[$smil parentNode] eq ""} {
	    set reparent 1
	} else {
	    [$smil parentNode] replaceChild $smil [$smil firstChild]
	}
    }
    if {[info exist reparent]} {
	set doc [dom parse [[$smil firstChild] asXML]]
    }
    return $doc
}

set t [expr {[lindex $argv 0] + 0.0}]
set result [applySMILtransform [dom parse [read stdin]] $t]
puts {<?xml version="1.0" ?>}
puts -nonewline [$result asXML -indent 2]
```

Note that <tt>input.smil</tt> contains the source document from the task description.

```txt

$ tclsh8.6 applySmil.tcl 0 < input.smil
<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color="1 1 1" location="0 2 0"/>
    <Shape>
      <Box size="2.0 1.0 2.0"/>
      <Appearance>
        <Material diffuseColor="0.0 0.6 1.0"/>
      </Appearance>
    </Shape>
  </Scene>
</X3D>
$ tclsh8.6 applySmil.tcl 2 < input.smil
<?xml version="1.0" ?>
<X3D>
  <Scene>
    <Viewpoint position="0 0 8" orientation="0 0 1 0"/>
    <PointLight color="1 1 1" location="0 2 0"/>
    <Shape>
      <Box size="1.8 1.2 1.8"/>
      <Appearance>
        <Material diffuseColor="0.2 0.5599999999999999 0.8"/>
      </Appearance>
    </Shape>
  </Scene>
</X3D>

```

