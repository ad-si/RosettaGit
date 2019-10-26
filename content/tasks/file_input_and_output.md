+++
title = "File input and output"
date = 1970-01-01
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
