%% Archivo: test_ttl.erl
-module(test7).
-export([start/0]).

start() ->
  TTL = 500, % TTL corto para la prueba (500 ms)
  Q = myqueue:start(testq_ttl, TTL),

  % Enviar mensaje sin consumidores (queda en ready)
  Q ! {msg, "msg_expira", none},
  io:format("Mensaje enviado a la cola con TTL ~p ms~n", [TTL]),

  % Esperar a que expire el mensaje
  timer:sleep(TTL + 200),

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
  ok.
