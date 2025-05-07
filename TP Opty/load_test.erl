-module(load_test).
-author("nicoalvarez").

%% API
-export([run/0]).

random_operations(Server, Pid, NumOps, StoreSize, SuccessfulCommits, FailedCommits) ->
  lists:foreach(
    fun(_) ->
      random_operation(Server, Pid, StoreSize),
      Ref = make_ref(),
      Pid ! {commit, Ref},
      receive
        {Ref, ok} ->
          io:format("Commit exitoso: ~p~n", [Ref]),
          ets:update_counter(SuccessfulCommits, count, 1);
        {Ref, abort} ->
          io:format("Transacción abortada por conflicto: ~p~n", [Ref]),
          ets:update_counter(FailedCommits, count, 1)
      end
    end,
    lists:seq(1, NumOps)
  ).

random_wait() ->
  % Esperar un tiempo aleatorio entre 0 y 100 ms
  WaitTime = random:uniform(150),
  timer:sleep(WaitTime).

random_operation(Server, Pid, StoreSize) ->
  RandomPosition = random:uniform(StoreSize),
  % Esperar un tiempo aleatorio
  random_wait(),
  Ref = make_ref(),
  io:format("Leyendo posición ~p~n", [RandomPosition]),
  Pid ! {read, Ref, RandomPosition},
  receive
    {Ref, ok, Value} ->
      io:format("Lectura exitosa: ~p~n", [Value])
  after 1000 ->
    io:format("Lectura fallida: Timeout~n")
  end,
  Write = random:uniform(100),
  io:format("Escribiendo en posición ~p valor ~p~n", [RandomPosition, Write]),
  %random_wait(),
  Pid ! {write, RandomPosition, Write}.

run() ->
  random:seed(erlang:monotonic_time()), % Inicializar semilla para aleatoriedad
  StoreSize = 30, % Tamaño del store
  Load = 10, % Reducir el número de procesos para concentrar los conflictos
  OpsPerProcess = 100, % Aumentar las operaciones por proceso para incrementar la probabilidad de conflicto
  Server = server:start(StoreSize),
  io:format("Iniciando carga de operaciones aleatorias con load ~p y ops ~p ~n", [Load, OpsPerProcess]),
  % Contadores para commits exitosos y fallidos
  SuccessfulCommits = ets:new(successful_commits, [named_table, public]),
  FailedCommits = ets:new(failed_commits, [named_table, public]),
  ets:insert(SuccessfulCommits, {count, 0}),
  ets:insert(FailedCommits, {count, 0}),
  lists:foreach(
    fun(_) ->
      spawn(fun() ->
        Pid = server:open(Server),
        random_operations(Server, Pid, OpsPerProcess, StoreSize, SuccessfulCommits, FailedCommits)
            end)
    end,
    lists:seq(1, Load)
  ),
  timer:sleep(Load * OpsPerProcess * 15), % Esperar a que todas las operaciones terminen
  Server ! stop,
  % Calcular y mostrar la tasa de fallos
  [{count, SuccessCount}] = ets:lookup(SuccessfulCommits, count),
  [{count, FailCount}] = ets:lookup(FailedCommits, count),
  TotalCommits = SuccessCount + FailCount,
  FailureRate = if
                  TotalCommits > 0 -> (FailCount * 100) / TotalCommits;
                  true -> 0
                end,
  io:format("Prueba finalizada con parametros
  StoreSize = ~p ,
  Load = ~p ,
  TotalCommits = ~p ,
  SuccessfulCommits = ~p ,
  FailedCommits = ~p ,
  Servidor detenido.~n", [StoreSize, Load, TotalCommits, SuccessCount, FailCount]),
  io:format("Tasa de fallos en commits: ~p\%~n", [FailureRate]).