 -module(test).

    -export([start/0, stop/0]).

 start() ->
     
     E1 = exchange:start("EX1"),
     register(w1, publisher:start("John", E1)).
  

    stop() ->
        stop(w1), stop(w2), stop(w3), stop(w4),
        io:format("Todos los Workers fueron detenidos~n").

    stop(Name) ->
        case whereis(Name) of
            undefined ->
                ok;
            Pid ->
                io:format("Deteniendo al worker ~p~n", [Name]),
                Pid ! stop
        end.
