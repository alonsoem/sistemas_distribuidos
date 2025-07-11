%% Archivo: test8.erl
-module(test8).
-export([start/0]).

start() ->
  TTL = 500, % TTL corto para la prueba (500 ms)
  DeadLetterQueue = myqueue:start(deadletter_queue),
  Q = myqueue:start(testq_ttl, TTL, DeadLetterQueue),

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
  % Validar que el mensaje expirado esté en la cola deadletter
  timer:sleep(500),
  DeadLetterState = myqueue:get_state(DeadLetterQueue),
  io:format("Estado de la cola deadletter:~n~p~n", [DeadLetterState]),
  case maps:get(ready, DeadLetterState, []) of
    [{"msg_expira", _}] ->
      io:format("OK: Mensaje expirado depositado en la cola deadletter~n");
    X ->
      io:format("ERROR: Mensaje expirado no encontrado en la cola deadletter~p~n", [X])
  end,

  ok.
