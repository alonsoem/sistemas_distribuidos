-module(server).
-export([start/1, open/1, stop/1]).

start(N) ->
  spawn(fun() -> init(N) end).

init(N) ->
  io:format("Iniciando server...~n"),
  Store = store:new(N),
  Validator = validator:start(Store),
  server(Validator, Store).

open(Server) ->
  Server ! {open, self()},
  receive
    {transaction, Validator, Store} ->
      handler:start(self(), Validator, Store)
  end.

server(Validator, Store) ->
  receive
    {open, Client} ->
      Client ! {transaction, Validator, Store}, % Enviar respuesta al cliente
      server(Validator, Store);
    stop ->
      store:stop(Store)
  end.

stop(Store) ->
  store:stop(Store),
  exit(normal).