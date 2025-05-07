-module(load_test).
-author("nicoalvarez").

%% API
-export([run/0]).

read_from_server(Server) ->
  Ref1 = make_ref(),
  Pid1 = server:open(Server),
  StartTime = erlang:monotonic_time(microsecond),
  Pid1 ! {read, Ref1, 1},
  receive
    {Ref1, ok, Value1} ->
      io:format("Lectura exitosa: ~p, microsegundos~n", [Value1])
  end,
  % Write number 10
  Ref2 = make_ref(),
  Pid1 ! {write, 1, 10},
  % Commit transaction
  Ref3 = make_ref(),
  Pid1 ! {commit, Ref3},
  receive
    {Ref3, ok} ->
      io:format("Commit exitoso: ~p~n", [Ref3]);
    {Ref3, abort} ->
      io:format("Transacción abortada por conflicto: ~p~n", [Ref3])
  end,
  EndTime = erlang:monotonic_time(microsecond),
  ResponseTime = EndTime - StartTime,
  ResponseTime.

run() ->
  Load = 1000,
  Server = server:start(3),
  io:format("Iniciando carga de lectura...~n"),
  StartTime = erlang:monotonic_time(second),
  ResponseTimes = lists:map(
    fun(_) ->
      read_from_server(Server)
    end,
    lists:seq(1, Load)
  ),
  EndTime = erlang:monotonic_time(second),
  io:format("Response times: ~p.~n", [length(ResponseTimes)]),
  AverageTime = lists:sum(ResponseTimes) div length(ResponseTimes),
  io:format("Tiempo promedio de respuesta: ~p microsegundos~n", [AverageTime])
  .
