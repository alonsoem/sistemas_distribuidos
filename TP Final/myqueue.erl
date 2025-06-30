-module(myqueue).
-export([start/1]).

start(Name) ->
  spawn(fun() -> init(Name, [], [], []) end).

% init(Name, Ready, Unacked, Consumers)
init(Name, Ready, Unacked, Consumers) ->
  receive
    {subscribe, From} ->
      From ! acksubsc,
      case Ready of
        [] ->
          init(Name, Ready, Unacked, [From | Consumers]);
        [Head | Tail] ->
          From ! {msg, Head},
          init(Name, Tail, [{From, Head} | Unacked], [From | Consumers])
      end;

    {msg, Body, _} ->
      io:format("Queue ~p recibio Mensaje  ~w~n", [Name, Body]),
      case Consumers of
        [] ->
          init(Name, [Body | Ready], Unacked, Consumers);
        [Head | Tail] ->
          Head ! {msg, Body},
          init(Name, Ready, [{Head, Body} | Unacked], Tail)
      end;

    {ack, Msg} ->
      NewUnacked = lists:filter(fun({_, M}) -> M =/= Msg end, Unacked),
      io:format("Mensaje ~p ACKed y eliminado~n", [Msg]),
      init(Name, Ready, NewUnacked, Consumers);

    {nack, Msg} ->
      % Buscar el consumidor y mensaje en Unacked, eliminarlo y devolver a Ready
      NewUnacked = lists:filter(fun({_, M}) -> M =/= Msg end, Unacked),
      io:format("Mensaje ~p NACKed, vuelve a la cola~n", [Msg]),
      init(Name, [Msg | Ready], NewUnacked, Consumers)
  end.
