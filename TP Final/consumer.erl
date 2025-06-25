-module(consumer).
-export([start/2]).

-define(timeout, 10000).

start(Name, Queue) ->
  spawn(fun() -> init(Name, Queue) end).

init(Name, Queue)->
  Queue ! {subscribe , self() },
  waitSubscriptionAck(Name,Queue).

waitSubscriptionAck(Name,Queue)->
  receive
    acksubsc -> 
    io:format("~p Suscripto! ~n", [Name]),
    loop(Name,Queue)
  after 2000 ->
    io:format("~p No pudo ser subscripto en tiempo ~n", [Name])
  end.

loop(Name, Queue) ->
  receive
    {msg,Body} -> 
      io:format("~p recibió el mensaje ~p~n", [Name,Body]),
      loop(Name, Queue);
    stop ->
      io:format("Deteniendo al consumer ~p~n", [Name]),
      ok
   
  end.

