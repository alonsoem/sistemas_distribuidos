-module(test6).
-export([start/0]).

start() ->
  Q = myqueue:start(testq),

  % Consumidor 1: suscribe antes de mensajes
  C1 = spawn(fun() ->
    Q ! {subscribe, self()},
    receive acksubsc -> ok end,
    receive
      {msg, M1} ->
        io:format("C1 recibe: ~p~n", [M1]),
        Q ! {ack, M1}
    end
             end),

  timer:sleep(100),

  % Enviar mensaje sin consumidores disponibles
  Q2 = myqueue:start(testq2),
  Q2 ! {msg, "msg_sin_consumidor", none},
  timer:sleep(100),
  C2 = spawn(fun() ->
    Q2 ! {subscribe, self()},
    receive acksubsc -> ok end,
    receive
      {msg, M2} ->
        io:format("C2 recibe: ~p~n", [M2]),
        Q2 ! {ack, M2}
    end
             end),

  timer:sleep(100),

  % Consumidor 3: NACK
  C3 = spawn(fun() ->
    Q ! {subscribe, self()},
    receive acksubsc -> ok end,
    receive
      {msg, M3} ->
        io:format("C3 recibe: ~p~n", [M3]),
        Q ! {nack, M3},
        receive
          {msg, M4} ->
            io:format("C3 recibe de nuevo: ~p~n", [M4]),
            Q ! {ack, M4}
        end
    end
             end),

  timer:sleep(100),

  % Enviar mensajes a la cola con consumidores
  Q ! {msg, "msg1", none},
  Q ! {msg, "msg2", none},

  timer:sleep(1000),

  ok.
