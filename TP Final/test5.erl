-module(test5).

-export([start/0]).

start() ->
  % Suscribirse a un mensaje y verificar que se recibe.
  E1 = exchange:start(ex1),
  Q1 = myqueue:start(q1),
  Q2 = myqueue:start(q2),
  Q3 = myqueue:start(q3),

  E1 ! { bind, Q1, rk1 },
  E1 ! { bind, Q2, rk2 },
  E1 ! { bind, Q3, rk3 },
  C1 = consumer:start("Paul",[Q1, Q2]),
  C2 = consumer:start("Ringo",[Q1]),
  E1 ! {msg, "Mensaje 1", rk1},

  E1 ! {msg, "Mensaje 2", rk2},

  E1 ! {msg, "Mensaje 3", rk3},

  timer:sleep(2000),

  C3 = consumer:start("Ringo",[Q3]).
  
