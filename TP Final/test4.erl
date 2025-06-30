-module(test4).
-export([start/0, stop/0]).

%%
%% - Publicar un mensaje ens un exchange con múltiples colas y verificar
%% que se envía a todas las colas.
start() ->
  E1 = exchange:start(ex1),
  Q1 = myqueue:start(q1),
  Q2 = myqueue:start(q2),
  Q3 = myqueue:start(q3),

  E1 ! { bind, Q1, rk1 },
  E1 ! { bind, Q2, rk1 },
  E1 ! { bind, Q3, rk1 },

  E1 ! {msg, "Mensaje 1", rk1}.

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
