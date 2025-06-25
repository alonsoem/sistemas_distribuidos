-module(myqueue).
-export([start/1]).

start(Name) ->
  spawn(fun() -> init(Name, [], []) end).

init(Name, Messages, Consumers) ->
  receive
    {subscribe , From} -> 
      From ! acksubsc,
      init(Name,Messages,[From | Consumers]);


    {msg, Body, _} ->
      io:format("Queue ~p recibio Mensaje  ~w~n", [Name, Body]),
      init(Name, [Body | Messages], Consumers)
  end.