+++
title = "HS Global Script"
description = ""
date = 2018-11-18T22:38:36Z
aliases = []
[extra]
id = 22082
[taxonomies]
categories = []
tags = []
+++



[Available here](https://github.com/jonathancast/hsglobalscript3); you will also need the language spec, [here](https://github.com/jonathancast/globalscript-spec).

This implementation is based on a translator for Global Script code embedded in a Haskell source file; to get a working program, you will need to add

```Haskell
import GSI.Env (runGSProgram)
$gsimports

main = runGSProgram $ [gs:value|
    -- Global Script code goes here
|]
```

around the program, then compile and run that with a Haskell compiler.
