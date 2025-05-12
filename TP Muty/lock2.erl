-module(lock2).
-export([start/1]).

start(Id) ->
  spawn(fun() -> init(Id) end).

init(Id) ->
  receive
    {peers, Peers} ->
      open(Id, Peers);
    stop ->
      ok
  end.

open(Id, Peers) ->
  receive
    {take, Master} ->
      Refs = requests(Id, Peers),
      wait(Id, Peers, Master, Refs, []);
    {request, From, FromId, Ref} ->
      From ! {ok, Ref},
      open(Id, Peers);
    stop ->
      ok
  end.

requests(Id, Peers) ->
  lists:map(fun(P) ->
    R = make_ref(),
    P ! {request, self(), Id, R},
    R
            end, Peers).

wait(Id, Peers, Master, [], Waiting) ->
  Master ! taken,
  held(Id, Peers, Waiting);

wait(Id, Peers, Master, Refs, Waiting) ->
  receive
    {ok, Ref} ->
      Refs2 = lists:delete(Ref, Refs),
      wait(Id, Peers, Master, Refs2, Waiting);

    {request, From, FromId, Ref} ->
      if
        FromId < Id ->
          % Prioridad más alta → ceder inmediatamente
          From ! {ok, Ref},
          wait(Id, Peers, Master, Refs, Waiting);
        true ->
          % Prioridad menor → responder después
          From ! {defer, Ref},
          wait(Id, Peers, Master, Refs, Waiting)
      end;

    release ->
      ok(Waiting),
      open(Id, Peers)
  after 5000 ->
    % Evitar bloqueos por tiempo de espera
    Master ! taken,
    held(Id, Peers, Waiting)
  end.

held(Id, Peers, Waiting) ->
  receive
    {request, From, _FromId, Ref} ->
      % Responder inmediatamente para evitar bloqueos
      From ! {ok, Ref},
      held(Id, Peers, Waiting);

    release ->
      ok(Waiting),
      open(Id, Peers)
  after 5000 ->
    % Liberar recursos automáticamente si no hay actividad
    ok(Waiting),
    open(Id, Peers)
  end.

ok(Waiting) ->
  lists:foreach(fun({From, Ref}) -> From ! {ok, Ref} end, Waiting).