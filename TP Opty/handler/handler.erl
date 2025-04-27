-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
  spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
  handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
      io:format("Handler init with pid: ~p  ~n", [self()]),
      receive
          % Maneja una operación de lectura
          {read, Ref, N} ->
              io:format("Handler: read operation for pid ~p~n", [self()]),
              % Busca si el valor solicitado ya está en la lista de Writes
              % Log writes list
              %io:format("Writes list: ~p~n", [Writes]),
              case lists:keysearch(N, 1, Writes) of
                  {value, {N, Value, _}} ->
                      % Si se encuentra, responde al cliente con el valor
                      Client ! {Ref, ok, Value},
                      % Llama recursivamente para continuar el loop
                      handler(Client, Validator, Store, Reads, Writes);
                  false ->
                      % Si no se encuentra, continúa el loop sin cambios
                      Client ! { notfound },
                      handler(Client, Validator, Store, Reads, Writes)
              end;
          % Maneja una nueva entrada de lectura
          {Ref, Entry, Value, Time} ->
              % Agrega la nueva lectura a la lista Reads y continúa el loop
              handler(Client, Validator, Store, [{Entry, Value, Time} | Reads], Writes);
          % Maneja una operación de escritura
          {write, N, Value} ->
              % Agrega la nueva escritura a la lista Writes con un timestamp
              Added = [{N, Value, now()} | Writes],
              % Continúa el loop con la lista actualizada
              handler(Client, Validator, Store, Reads, Added);
          % Maneja una solicitud de commit
          {commit, Ref} ->
              % Envía un mensaje al Validator para validar las lecturas y escrituras
              Validator ! {validate, Ref, Reads, Writes, Client};
          % Maneja una solicitud de abortar la transacción
          abort ->
              % Finaliza el proceso sin realizar más acciones
              ok
      end.