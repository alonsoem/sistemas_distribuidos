 -module(lock4).

    -export([start/1]).

    start(Id) ->
        spawn(fun() -> init(Id) end).

    init(_) ->
        receive
            {peers, Peers} ->
                open(Peers);
            stop ->
                ok
        end.

    open(Nodes) ->
        receive
            {take, Master} ->
                %Al recibir intencion de tomar la seccion critica le aviso a los otros locks
                Refs = requests(Nodes),
                wait(Nodes, Master, Refs, []);
            {request, From, Ref} ->
                From ! {ok, Ref},
                open(Nodes);
            stop ->
                ok
        end.

    requests(Nodes) ->
        lists:map(fun(P) ->
                          R = make_ref(), 
                          P ! {request, self(), R},
                          R
                  end, Nodes).

    wait(Nodes, Master, [], Waiting) ->
        Master ! taken,
        held(Nodes, Waiting);

    wait(Nodes, Master, Refs, Waiting) ->
        %Espero los oks pero no puedo recibir un take porque estoy procesando uno
        receive
            {request, From, Ref} ->
                wait(Nodes, Master, Refs, [{From, Ref}|Waiting]);
            {ok, Ref} ->
                Refs2 = lists:delete(Ref, Refs),
                wait(Nodes, Master, Refs2, Waiting);
            release ->
                ok(Waiting),
                open(Nodes)
       end.

    ok(Waiting) ->
        lists:foreach(fun({F,R}) -> F ! {ok, R} end, Waiting).


 held(Nodes, Waiting) ->
        receive
            
            release ->
                ok(Waiting),
                open(Nodes)
        end.

