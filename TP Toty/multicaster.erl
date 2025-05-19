-module(multicaster).
-export([start/0]).
-define(timeout, 5000).

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
                            io:format("Registering ~p~n",[Node]),
                            Node ! registered,
                            init(Order, [Node | Nodes]);

            stop ->
                    stop

        after ? timeout ->    
            sendMessages(Nodes),
            init(Order,Nodes)
            
                            

        end.

    sendMessages(Nodes) ->
        lists:map(fun(P) ->
                          R = make_ref(), 
                          P ! {msg, R},
                          R
                  end, Nodes).
