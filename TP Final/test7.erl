%% Archivo: test7.erl
-module(test7).
-export([start/0]).

start() ->
  TTL = 500, % TTL corto para la prueba (500 ms)
  Q = myqueue:start(testq_ttl, TTL),

  % Enviar mensaje sin consumidores (queda en ready)
  Q ! {msg, "msg_expira", none},
  io:format("Mensaje enviado a la cola con TTL ~p ms~n", [TTL]),
  io:format("Estado inicial de la cola:~n~p~n", [myqueue:get_state(Q)]),

  % Esperar a que expire el mensaje
  timer:sleep(TTL + 200),
  io:format("Estado tras expiración del mensaje:~n~p~n", [myqueue:get_state(Q)]),

  % Suscribir consumidor después del vencimiento
  C = spawn(fun() ->
    Q ! {subscribe, self()},
    receive
      acksubsc -> ok
    end,
    receive
      {msg, M} ->
        io:format("ERROR: Consumidor recibió mensaje expirado: ~p~n", [M])
    after 500 ->
      io:format("OK: Consumidor no recibió mensaje (expirado)~n", [])
    end
            end),

  timer:sleep(1000),
  GuiPid = queue_gui:start("gui", Q),
  Q2 = myqueue:start(testq_ttl2, TTL),
  queue_gui:start("gui2", Q2),
  %GuiPid ! {estado, myqueue:get_state(Q)},
ok.
