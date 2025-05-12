 -module(lock2).

    -export([start/1]).

    start(Id) ->
        spawn(fun() -> init(Id) end).

    init(Priority) ->
        receive
            {peers, Peers} ->
                open(Peers,Priority);
            stop ->
                ok
        end.

    open(Nodes,MyPriority) ->
        receive
            {take, Master} ->
                Refs = requests(Nodes, MyPriority),
                wait(Nodes, Master, Refs, [], MyPriority);
            {request, From, Ref, Priority} ->
                io:format("request priority, mypriority: ~w ~w ~n", [Priority,MyPriority]),
                if
                    MyPriority > Priority ->
                        From ! {ok, Ref}
                end,
                open(Nodes,MyPriority);
                
            stop ->
                ok
        end.

    requests(Nodes, Priority) ->
        lists:map(fun(P) ->
                          R = make_ref(), 
                          P ! {request, self(), R, Priority},
                          R
                  end, Nodes).

    wait(Nodes, Master, [], Waiting, MyPriority) ->
        Master ! taken,
        held(Nodes, Waiting, MyPriority);

    wait(Nodes, Master, Refs, Waiting, MyPriority) ->
        receive
            {request, From, Ref} ->
                wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyPriority);
            {ok, Ref} ->
                Refs2 = lists:delete(Ref, Refs),
                wait(Nodes, Master, Refs2, Waiting, MyPriority);
            release ->
                ok(Waiting),
                open(Nodes, MyPriority)
       end.

    ok(Waiting) ->
        lists:foreach(fun({F,R}) -> F ! {ok, R} end, Waiting).


 held(Nodes, Waiting, MyPriority) ->
        receive
            {request, From, Ref} ->
                held(Nodes, [{From, Ref}|Waiting], MyPriority);
            release ->
                ok(Waiting),
                open(Nodes, MyPriority)
        end.

