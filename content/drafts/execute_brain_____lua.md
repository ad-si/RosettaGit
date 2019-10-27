+++
title = "Execute Brain****/Lua"
description = ""
date = 2016-09-01T18:07:19Z
aliases = []
[extra]
id = 5392
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}

An implementation of a Brainf*** interpreter in [[Lua]].

```lua
memory = {0} --memory is bounded on one side, at 1

program = io.read("*all")

pointer = 1

instruction = 1

retpoints = {}

functions = {
[">"] = function()
  pointer = pointer + 1
  if not memory[pointer] then memory[pointer] = 0 end
end,
["<"] = function()
  if pointer == 1 then error"Memory out-of-bounds!" end
  pointer = pointer - 1
end,
["+"] = function()
  memory[pointer] = memory[pointer] + 1
end,
["-"] = function()
  memory[pointer] = memory[pointer] - 1
end,
["["] = function()
  if memory[pointer] ~= 0 then
    table.insert(retpoints, instruction)
  else -- if the memory at the pointer is zero, jump to the matching close bracket
    local b = 1 -- b stores number of unclosed brackets (when b == 0 the match has been found)
    while instruction <= #program and b ~= 0 do
      instruction = instruction + 1
      if program:sub(instruction, instruction) == "[" then
        b = b + 1
      elseif program:sub(instruction, instruction) == "]" then
        b = b - 1
      end
    end
    if b ~= 0 then
      error"Missing ']'!"
    end
  end
end,
["]"] = function()
  if #retpoints > 0 then
    if memory[pointer] ~= 0 then
      instruction = retpoints[#retpoints]
    else
      table.remove(retpoints)
    end
  else
    error"Missing '['!"
  end
end,
["."] = function()
  io.write(string.char(memory[pointer]))
end,
[","] = function()
  memory[pointer] = io.read():byte()
end}
while instruction <= #program do
  local instr = functions[program:sub(instruction,instruction)]
  if instr then instr() end
  instruction = instruction + 1
end
```

