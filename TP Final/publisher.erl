-module(publisher).
-export([start/3]).

-define(timeout, 10000).

start(Name, Exchange, RoutingKey) ->
  Sleep = rand:uniform(2000),
  spawn(fun() -> init(Name, Exchange, Sleep, RoutingKey) end).

init(Name, Exchange, Sleep, RoutingKey) ->
  receive
    stop ->
      io:format("Deteniendo al publisher ~p~n", [Name]),
      ok;
    last ->
      Exchange ! { last, self() },
      init(Name, Exchange, Sleep, RoutingKey)
after 2000 ->
    send(Exchange, Sleep, RoutingKey, "Test message from " ++ Name),
    init(Name, Exchange, Sleep, RoutingKey)
end.

send(Exchange, Sleep, RoutingKey, Body) ->
  timer:sleep(rand:uniform(Sleep)),
  Exchange ! {msg, RoutingKey, Body }.