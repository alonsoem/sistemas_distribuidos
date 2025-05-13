-module(lock3).
-import(time,[zero/0,inc/0]).
-export([start/1]).

start(Id) ->
  Clock=time:zero(),
  spawn(fun() -> init(Id, Clock) end).

init(Id, Clock) ->
  receive
    {peers, Peers} ->
      open(Id, Peers, Clock);
    stop ->
      ok
  end.

open(Id, Peers, Clock) ->
  receive
    {take, Master} ->
      Refs = requests(Id, Peers, Clock),
      wait(Id, Peers, Master, Refs, [], Clock);

    {request, From, FromId, Ref, FromClock} ->
        UpdatedClock=time:inc(Clock),
        if 
          UpdatedClock > FromClock ->
                      From ! {ok, Ref},
                      open(Id, Peers, UpdatedClock);

          UpdatedClock < FromClock ->
                      From ! {ok, Ref},
                      open(Id, Peers, FromClock);
          true ->
                      if
                        FromId < Id ->
                          % Prioridad más alta → ceder inmediatamente
                          From ! {ok, Ref};

                        true ->
                          % Prioridad menor → responder después
                          From ! {defer, Ref}                     
                          
                      end,
                      open(Id, Peers, UpdatedClock)
        end;
        
        
      
    stop ->
      ok
  end.

requests(Id, Peers,Clock) ->
  %No deberia actualizar clock con cada envio?
  lists:map(fun(P) ->
    R = make_ref(),
    P ! {request, self(), Id, R, Clock},
    R
            end, Peers).

wait(Id, Peers, Master, [], Waiting, Clock) ->
  Master ! taken,
  held(Id, Peers, Waiting, Clock);

wait(Id, Peers, Master, Refs, Waiting, Clock) ->
  receive
    {ok, Ref} ->
      Refs2 = lists:delete(Ref, Refs),
      wait(Id, Peers, Master, Refs2, Waiting, Clock);

    {request, From, FromId, Ref} ->
      if
        FromId < Id ->
          % Prioridad más alta → ceder inmediatamente
          From ! {ok, Ref},
          wait(Id, Peers, Master, Refs, Waiting, Clock);
        true ->
          % Prioridad menor → responder después
          From ! {defer, Ref},
          wait(Id, Peers, Master, Refs, Waiting, Clock)
      end;

    release ->
      ok(Waiting),
      open(Id, Peers, Clock)
  after 5000 ->
    % Evitar bloqueos por tiempo de espera
    Master ! taken,
    held(Id, Peers, Waiting, Clock)
  end.

held(Id, Peers, Waiting, Clock) ->
  receive
    {request, From, _FromId, Ref} ->
      % Responder inmediatamente para evitar bloqueos
      From ! {ok, Ref},
      held(Id, Peers, Waiting, Clock);

    release ->
      ok(Waiting),
      open(Id, Peers, Clock)
  after 5000 ->
    % Liberar recursos automáticamente si no hay actividad
    ok(Waiting),
    open(Id, Peers, Clock)
  end.

ok(Waiting) ->
  lists:foreach(fun({From, Ref}) -> From ! {ok, Ref} end, Waiting).