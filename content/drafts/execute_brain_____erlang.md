+++
title = "Execute Brain****/Erlang"
description = ""
date = 2010-08-18T18:39:52Z
aliases = []
[extra]
id = 4944
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
Implementation of a [[Brainfuck]] interpreter in [[Erlang]].

Normal brainfuck implementations rely on the use of arrays to store data and increment pointers. Given Erlang is a functional language and its usual array structures are relatively slow, this version relies on binaries and bit syntax. Binaries can be iterated through in linear time, but you can jump through them at any point in constant time. This makes binaries ideal for storing data as a tape and moving from cell to cell in one way or another, where standard lists would be inefficient.

The opcodes also use a binary representation, which allows to jump through the loop structures. Note, however, that finding the end of the loop still requires linear time, which could be improved.


```erlang
-module(brainfuck).
-export([run/1]).

-define(Tape, <<0:30000/unit:8>>). % unit =:= cell size
-define(DATA_POINTER, 0).

run(Instructions) when is_list(Instructions) ->
    run(list_to_binary(Instructions));
run(Instructions) when is_binary(Instructions) ->
    run(Instructions, ?DATA_POINTER, ?Tape),
    io:format("~n").

%% Ins = instructions,
%% DataPtr = Previous index,
%% Tape = Data
run(<< $>, Ins/binary>>, DataPtr, Tape) ->
    run(Ins, DataPtr+1, Tape);
run(<< $<, Ins/binary>>, DataPtr, Tape) ->
    run(Ins, DataPtr-1, Tape);
run(<< $+, Ins/binary>>, DataPtr, Tape) ->
    <<Prev:(DataPtr)/binary, X/integer, Next/binary>> = Tape,
    run(Ins, DataPtr, <<Prev/binary, (X+1)/integer, Next/binary>>);
run(<< $-, Ins/binary>>, DataPtr, Tape) ->
    <<Prev:(DataPtr)/binary, X/integer, Next/binary>> = Tape,
    run(Ins, DataPtr, <<Prev/binary, (X-1)/integer, Next/binary>>);
run(<< $., Ins/binary>>, DataPtr, Tape) ->
    <<_:(DataPtr)/binary, X/integer, _/binary>> = Tape,
    io:put_chars([X]),
    run(Ins, DataPtr, Tape);
run(<< $,, Ins/binary>>, DataPtr, Tape) ->
    <<Prev:(DataPtr)/binary, _/integer, Next/binary>> = Tape,
    X = case io:get_chars("",1) of
            [C] -> C;
            eof -> -1
        end,
    run(Ins, DataPtr, <<Prev/binary, X/integer, Next/binary>>);
run(<< $[, Ins/binary>>, DataPtr, Tape) ->
    End = find_end(Ins),
    <<Loop:End/binary, PostLoop/binary>> = Ins,
    <<_:(DataPtr)/binary, X/integer, _/binary>> = Tape,
    if X =:= 0 ->
        run(PostLoop, DataPtr, Tape);
       X =/= 0 ->
        {NewPtr, NewTape} = run(Loop, DataPtr, Tape),
        run(<<$[,Loop/binary,$],PostLoop/binary>>, NewPtr, NewTape)
    end;
run(<<_, Ins/binary>>, DataPtr, Tape) ->
    run(Ins, DataPtr, Tape);
run(<<>>, DataPtr, Tape) -> {DataPtr, Tape}.


%% finds the point where a loop ends.
find_end(Ins) ->
    find_end(Ins, 0, 0).

find_end(<< $], _/binary>>, 0, Pos) ->
    Pos+1;
find_end(<< $], Ins/binary>>, N, Pos) ->
    find_end(Ins, N-1, Pos+1);
find_end(<< $[, Ins/binary>>, N, Pos) ->
    find_end(Ins, N+1, Pos+1);
find_end(<<_, Ins/binary>>, N, Pos) ->
    find_end(Ins, N, Pos+1).
```


Example output, the first one being the usual 'Hello World!' program and the second one testing nested loops to increment count to 64 and print the result (should be '@'):


```txt

1> c(brainfuck).
{ok,brainfuck}
2> brainfuck:run("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.").
Hello World!

ok
3> brainfuck:run(">>++++[<++++[<++++>-]>-]<<.[-]++++++++++.").
@                                                   

ok


```

