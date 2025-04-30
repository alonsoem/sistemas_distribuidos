-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
  spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
  handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
      receive
          % Maneja una operación de lectura
          {read, Ref, N} ->
              case lists:keysearch(N, 1, Writes) of
                  {value, {N, Value, _}} ->
                      % Si se encuentra, responde al cliente con el valor
                      Client ! {Ref, ok, Value},
                      handler(Client, Validator, Store, Reads, Writes);
                  false ->
                      % Si no se encuentra en Writes, busca en Store
                      case store:lookup(N, Store) of
                          Entry ->
                              % Si se encuentra en Store, responde al cliente
                              Entry ! {read, Ref, self()},
                              % Llama recursivamente para continuar el loop
                              handler(Client, Validator, Store, Reads, Writes);
                          _ ->
                              % Si no se encuentra en Store, envía notfound
                              Client ! {Ref, notfound},
                              % Llama recursivamente para continuar el loop
                              handler(Client, Validator, Store, Reads, Writes)
                      end,
                      % Si no se encuentra, continúa el loop sin cambios
                      Client ! { notfound },
                      handler(Client, Validator, Store, Reads, Writes)
              end;
          % Maneja una nueva entrada de lectura
          {Ref, Entry, Value, Time} ->
              % Agrega la nueva lectura a la lista Reads y continúa el loop
              Client ! {Ref, ok, Value},
              handler(Client, Validator, Store, [{Entry, Time} | Reads], Writes);
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