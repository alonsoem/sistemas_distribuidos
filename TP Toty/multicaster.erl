-module(multicaster).
-export([start/0]).

    start() ->
        Order=1,
        spawn(fun() -> init(Order,[]) end).
    
    init(Order, Nodes) ->
        receive
             {send, Msg} ->
                            init(Order,Nodes);

                            %Ref = make_ref(),
                           % request(?, ?, ?, ?),
                            %Cast2 = cast(?, ?, ?),
                            %server(Master, Next, Nodes, Cast2, Queue, Jitter);

            {register,Node} ->
                io:format("register ~p~n",[Node]),
                            Node ! registered,
                            init(Order,Nodes)
                            

        end.
