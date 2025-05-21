 -module(test).

    -export([start/0, stop/0]).

 start() ->
     % Iniciar y registrar multicasters
     M1 = isis_multicaster:start(),
     M2 = isis_multicaster:start(),
     M3 = isis_multicaster:start(),
     M4 = isis_multicaster:start(),

     % Registrar multicasters entre sí
     M1 ! {registerMulticaster, M2},
     M1 ! {registerMulticaster, M3},
     M1 ! {registerMulticaster, M4},
     M2 ! {registerMulticaster, M1},
     M2 ! {registerMulticaster, M3},
     M2 ! {registerMulticaster, M4},
     M3 ! {registerMulticaster, M1},
     M3 ! {registerMulticaster, M2},
     M3 ! {registerMulticaster, M4},
     M4 ! {registerMulticaster, M1},
     M4 ! {registerMulticaster, M2},
     M4 ! {registerMulticaster, M3},

     % Iniciar y registrar workers con su multicaster correspondiente
     register(w1, worker:start("John", M1)),
     register(w2, worker:start("Ringo", M2)),
     register(w3, worker:start("Paul", M3)),
     register(w4, worker:start("George", M4)).


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
