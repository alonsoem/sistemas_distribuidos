-module(consumer).
-export([start/2]).

-define(timeout, 10000).

start(Name, Queue) ->
  spawn(fun() -> init(Name, Queue) end).

init(Name, Queue)->
  multicastSubscription(Queue),
  waitSubscriptionAck(Name,Queue).


multicastSubscription(Queue)->
  case Queue of
    []->
      ok;
    
    [Head|Tail]->
      Head ! {subscribe , self() },
      multicastSubscription(Tail)
  end.
  
waitSubscriptionAck(Name,Queue)->
  receive
    acksubsc -> 
    io:format("~p suscripto! ~n", [Name]),
    loop(Name,Queue)
  after 2000 ->
    io:format("~p No pudo ser subscripto a al menos una Queue a tiempo ~n", [Name])
  end.

loop(Name, Queue) ->
  receive
    acksubsc -> 
      io:format("~p suscripto! ~n", [Name]),
      loop(Name,Queue);
    {msg,Body} -> 
      io:format("~p recibió el mensaje ~p~n", [Name,Body]),
      loop(Name, Queue);
    stop ->
      io:format("Deteniendo al consumer ~p~n", [Name]),
      ok
   
  end.

