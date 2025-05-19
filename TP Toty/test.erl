 -module(test).

    -export([start/1, stop/0]).

    start(Multicast) ->
        

        register(w1, worker:start("John", Multicast )),
        register(w2, worker:start("Ringo", Multicast)),
        register(w3, worker:start("Paul", Multicast)),
        register(w4, worker:start("George", Multicast)).
        
        
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
