% test6.erl
-module(test6).
-export([start/0]).

start() ->
  E1 = exchange:start(ex1),
  Q1 = myqueue:start(q1),
  E1 ! {bind, Q1, rk1},

  % Consumidor que hace NACK
  spawn(fun() ->
    Q1 ! {subscribe, self()},
    receive acksubsc -> ok end,
    receive
      {msg, Msg} ->
        io:format("Consumidor 1 recibe: ~p~n", [Msg]),
        timer:sleep(500),
        Q1 ! {nack, Msg}
    end
        end),

  % Consumidor que hace ACK
  spawn(fun() ->
    Q1 ! {subscribe, self()},
    receive acksubsc -> ok end,
    receive
      {msg, Msg} ->
        io:format("Consumidor 2 recibe: ~p~n", [Msg]),
        timer:sleep(500),
        Q1 ! {ack, Msg}
    end
        end),

  % Publicar mensaje
  E1 ! {msg, "Mensaje para ACK/NACK", rk1},
  timer:sleep(2000).
