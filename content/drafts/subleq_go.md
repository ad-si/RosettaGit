+++
title = "Subleq/go"
description = ""
date = 2015-04-27T16:28:45Z
aliases = []
[extra]
id = 19063
[taxonomies]
categories = []
tags = []
+++

A much longer Go version for [[Subleq#Go]].

This version would be more appropriate if this was the start of a more complicated virtual machine
or if multiple different virtual machines were to be supported.
E.g. perhaps via a simple interface like:

```go
type VirtualMachine interface {
	Run()  error
	Step() error
	Err()  error
}
```



```go
package main

import (
	"bufio"
	"errors"
	"flag"
	"io"
	"log"
	"os"
	"strconv"
)

type word int // or int8, int64, whatever

// A Subleq represents the entire state of a virtual Subleq machine.
type Subleq struct {
	mem    []word // memory contents
	ip     int    // instruction pointer
	Stdin  io.ByteReader
	Stdout io.Writer // or io.ByteWriter, but using bufio.Writer requires flushing
	err    error
}

// Errors produced by Step or Run methods.
var (
	ErrHalt = errors.New("halted")
)

// New returns a new Subleq machine instance with the specified
// program loaded into memory.
// The machine's Stdin and Stdout are set to os.Stdin and os.Stdout.
func New(prog []word) *Subleq {
	return &Subleq{
		mem:    prog,
		Stdin:  bufio.NewReader(os.Stdin),
		Stdout: os.Stdout,
	}
}

func NewFrom(r io.Reader) (*Subleq, error) {
	var prog []word
	scan := bufio.NewScanner(r)
	scan.Split(bufio.ScanWords)
	for scan.Scan() {
		v, err := strconv.Atoi(scan.Text())
		if err != nil {
			return nil, err
		}
		prog = append(prog, word(v))
	}
	if err := scan.Err(); err != nil {
		return nil, err
	}
	return New(prog), nil
}

func (s Subleq) Err() error { return s.err }
func (s *Subleq) Run() error {
	for s.Step() == nil {
	}
	return s.Err()
}

func (s *Subleq) Step() (err error) {
	defer func() {
		// In particular, turn runtime "index out of range"
		// panics into plain errors.
		if r := recover(); r != nil {
			if e, ok := r.(error); ok && err == nil {
				err = e
			} else {
				panic(r)
			}
		}
		// save errors other than halt
		if s.err == nil && err != nil && err != ErrHalt {
			s.err = err
		}
	}()
	if s.err != nil {
		return s.err
	}
	if s.ip < 0 {
		return ErrHalt
	}
	op := s.ReadOp()
	switch {
	case op.a == -1:
		var c byte
		c, err = s.Stdin.ReadByte()
		if err == nil {
			s.mem[op.b] = word(c)
		}
	case op.b == -1:
		//err = s.Stdout.WriteByte(byte(s.mem[op.a]))
		_, err = s.Stdout.Write([]byte{byte(s.mem[op.a])})
	default:
		b := s.mem[op.b] - s.mem[op.a]
		s.mem[op.b] = b
		if b <= 0 {
			s.ip = int(op.c)
		}
	}
	return err
}

func (s *Subleq) ReadOp() operands {
	op := operands{
		a: s.mem[s.ip],
		b: s.mem[s.ip+1],
		c: s.mem[s.ip+2],
	}
	s.ip += 3
	return op
}

type operands struct{ a, b, c word }

func main() {
	// default program
	var prog = []word{
		15, 17, -1,
		17, -1, -1,
		16, 1, -1,
		16, 3, -1,
		15, 15, 0,

		0, -1, 72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10, 0,
		// or:
		// 0, -1,
		// 'H', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '!', '\n',
		// 0,
	}
	var s *Subleq
	flag.Parse()
	switch flag.NArg() {
	case 1:
		filename := flag.Arg(0)
		f, err := os.Open(filename)
		if err != nil {
			log.Fatal(err)
		}
		s, err = NewFrom(f)
		f.Close()
		if err != nil {
			log.Fatal(err)
		}
	default:
		prog = prog[:0]
		for _, arg := range flag.Args() {
			v, err := strconv.Atoi(arg)
			if err != nil {
				log.Fatal(err)
			}
			prog = append(prog, word(v))
		}
		fallthrough
	case 0:
		s = New(prog)
	}
	if err := s.Run(); err != nil {
		log.Fatalln("error:", err)
	}
}
```

