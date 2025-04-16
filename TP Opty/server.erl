-module(server).
-export([start/1, open/1, stop/1]).

start(N) ->
  spawn(fun() -> init(N) end).

init(N) ->
  io:format("Iniciando server...~n"),
  Store = store:new(N),
  io:format("Store inicializado com ~p entradas~n", [N]),
  Validator = validator:start(Store),
  io:format("Validator inicializado com store ~p~n", [Store]),
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
      server(Validator, Store);
    stop ->
      store:stop(Store)
  end.

stop(Store) ->
  store:stop(Store),
  exit(normal).