 -module(test).

    -export([start/0, stop/0]).

    start() ->
        Multicast= apply(multicaster, start, []),

        register(w1, worker:start("John", Multicast )),
        %register(w2, worker:start("Ringo", Multicaster)),
        %register(w3, worker:start("Paul", Multicaster)),
        %register(w4, worker:start("George", Multicaster)),
        ok.

    stop() ->
        stop(w1), %stop(w2), stop(w3), stop(w4),
        io:format("Todos los Workers fueron detenidos~n").

    stop(Name) ->
        case whereis(Name) of
            undefined ->
                ok;
            Pid ->
                io:format("Deteniendo al worker ~p~n", [Name]),
                Pid ! stop
        end.
