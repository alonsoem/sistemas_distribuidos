-module(test_flow).
-export([run/0]).

run() ->
  io:format("Iniciando pruebas de flujo completo...~n"),
  % Inicializar el servidor con un store de tamaño 3
  Server = server:start(3),
  Ref1 = make_ref(),
  % Caso 1: Transacción sin conflictos
  io:format("Caso 1: Transacción sin conflictos~n"),
  Pid1 = server:open(Server),
  Pid1 ! {write, 1, 55},
  Pid1 ! {read, Ref1, 1},
  receive
    {Ref1, ok, Value1} ->
      io:format("Lectura exitosa: ~p~n", [Value1])
  end,
  Ref2 = make_ref(),
  Pid1 ! {commit, Ref2},
  receive
    {Ref2, ok} ->
      io:format("Commit exitoso.~n")
  end,
  % Caso 2: Transacción con conflicto de escritura
  io:format("Caso 2: Transacción con conflicto de escritura~n"),
  T0 = server:open(Server),
  T0 ! {write, 1, 100},
  Ref3 = make_ref(),
  T0 ! { commit , Ref3},
  receive
    {Ref3, ok} ->
      io:format("Commit exitoso T0.~n")
  end,

  T2 = server:open(Server),
  T2 ! {write, 1, 102},

  T3 = server:open(Server),
  Ref5 = make_ref(),
  T3 ! {read, Ref5, 1},
  receive
  {Ref5, ok, Value2} ->
    io:format("Lectura exitosa ReadValue: ~p~n", [Value2]),
    T3 ! {write, 1, Value2 + 1}
end,
  T2 ! {commit, Ref5},
  receive
  {Ref5, ok} ->
    io:format("Transacción 2 commit exitoso.~n")
end,
  Ref6 = make_ref(),
  T3 ! {commit, Ref6},
  receive
    {Ref6, abort} ->
      io:format("Transacción 3 abortada por conflicto.~n");
    {Ref6, ok} ->
      io:format("Transacción 3 commit exitoso.~n")
  end,

  % Detener el servidor
  Server ! stop,
  io:format("Servidor detenido.~n").