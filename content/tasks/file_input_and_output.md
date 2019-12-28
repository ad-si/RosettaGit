+++
title = "File input and output"
date = 1970-01-01
[extra]
task = """
  Create a file called "output.txt",
  and place in it the contents of the file "input.txt"
  via an intermediate variable.
"""
[taxonomies]
categories = ["task"]
tags = ["basic", "cli"]
languages = ["haskell", "rust"]
+++

## [Haskell](/languages/haskell)

**Note:**
This doesn't keep the file in memory.
Buffering is provided by lazy evaluation.

```haskell
main = readFile "input.txt" >>= writeFile "output.txt"
```


## [Python](/languages/python)

```python
i = open("input.txt", "r")
o = open("output.txt", "w+")
o.write(i.read())
```



## [Rust](/languages/rust)

**Note:**
The program will panic with any sort of error.

```rust
use std::fs::File;
use std::io::{Read, Write};

fn main() {
  let mut file = File::open("input.txt").unwrap();
  let mut data = Vec::new();
  file.read_to_end(&mut data).unwrap();
  let mut file = File::create("output.txt").unwrap();
  file.write_all(&data).unwrap();
}
```
