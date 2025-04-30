-module(clock).
-export([start/1]).


start(Nodes)->
    Zero=length(Nodes),
    spawn_link(fun() -> init(Zero) end).

stop() ->
    stop.


init(Time)->
    io:format("log time clock: ~w ~n", [Time]).


