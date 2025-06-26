-module(test5).

-export([start/0]).

start() ->
  % Suscribirse a un mensaje y verificar que se recibe.
  E1 = exchange:start(ex1),
  Q1 = myqueue:start(q1),
  E1 ! { bind, Q1, rk1 },
  C1 = consumer:start("Paul",Q1),
  E1 ! {msg, "Mensaje 1", rk1}.

  
