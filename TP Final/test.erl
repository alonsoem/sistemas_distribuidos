-module(test).

-export([start/0, stop/0]).

start() ->
  E1 = exchange:start(ex1),
  Q1 = myqueue:start(q1),
  E1 ! { bind, Q1, rk1 },

  % Envia mensajes de prueba cada 2000ms
  %register(w1, publisher:start(john, E1, rk1)).

  E1 ! {msg, "Mensaje 1", rk1},

  E2 = exchange:start(ex2),
  E1 ! { bind, E2, rk1 },

  E1 ! { get_bindings, self() },
  receive
    {bindings, Bindings} ->
      io:format("Bindings en E1: ~p~n", [Bindings])
  end,

  E1 ! { msg, "Mensaje 2", rk1}.

stop() ->
  stop(w1),
  io:format("Todos los Workers fueron detenidos~n").

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      io:format("Deteniendo al worker ~p~n", [Name]),
      Pid ! stop
  end.
