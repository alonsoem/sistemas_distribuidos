-module(test_flow).
-export([run/0]).

run() ->
  io:format("Iniciando pruebas de flujo completo...~n"),
  % Inicializar el servidor con un store de tamaño 3
  Server = server:start(3),

  % Caso 1: Transacción sin conflictos
  io:format("Caso 1: Transacción sin conflictos~n"),
  Pid1 = server:open(Server),
  io:format("Caso 1: is_process_alive(Pid1) ~p~n", [is_process_alive(Pid1)]),
  Pid1 ! {read, ref1, 1},
  receive
    {ref1, ok, Value1} ->
      io:format("Lectura exitosa: ~p~n", [Value1])
  end,
  Pid1 ! {write, 1, 42},
  Pid1 ! {commit, ref2},
  receive
    {ref2, ok} ->
      io:format("Commit exitoso.~n")
  end,

  % Caso 2: Transacción con conflicto de escritura
  io:format("Caso 2: Transacción con conflicto de escritura~n"),
  Pid2 = server:open(Server),
  Pid3 = server:open(Server),
  Pid2 ! {read, ref3, 1},
  Pid3 ! {read, ref4, 1},
  receive
    {ref3, ok, Value2} ->
      io:format("Transacción 2 lectura exitosa: ~p~n", [Value2])
  end,
  receive
    {ref4, ok, Value3} ->
      io:format("Transacción 3 lectura exitosa: ~p~n", [Value3])
  end,
  Pid2 ! {write, 1, 100},
  Pid3 ! {write, 1, 200},
  Pid2 ! {commit, ref5},
  receive
    {ref5, ok} ->
      io:format("Transacción 2 commit exitoso.~n")
  end,
  Pid3 ! {commit, ref6},
  receive
    {ref6, abort} ->
      io:format("Transacción 3 abortada por conflicto.~n")
  end,

  % Detener el servidor
  Server ! stop,
  io:format("Servidor detenido.~n").