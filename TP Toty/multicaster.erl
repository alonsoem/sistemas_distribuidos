-module(multicaster).
-export([start/0]).
-define(timeout, 1000).

    start() ->
        Order=1,
        spawn(fun() -> init(Order,[],[]) end).
    
    init(Order, Nodes, Queue) ->
        receive
             {msg, Id, From} ->
                            init(Order,Nodes,[{Id,From} | Queue]);

                            %Ref = make_ref(),
                           % request(?, ?, ?, ?),
                            %Cast2 = cast(?, ?, ?),
                            %server(Master, Next, Nodes, Cast2, Queue, Jitter);

            {register,Node} ->
                            io:format("Registering ~p~n",[Node]),
                            Node ! registered,
                            init(Order, [Node | Nodes],Queue);

            stop ->
                    stop

        after ? timeout ->    
            NewQueue = readMessages(Nodes,Queue),
            init(Order,Nodes,NewQueue)
            
                            

        end.





    readMessages(Nodes, Queue)->
        SortedQueue= lists:keysort(1, Queue),            
        if SortedQueue==[] ->
                [];
            true ->
                Head=hd(SortedQueue),
                sendMessages(Nodes,Head),
                tl(SortedQueue)

        end.
            
       


    sendMessages(Nodes,Message) ->

        lists:map(fun(Node) ->
                          
                          Node ! {msg, Message},
                          ok
                  end, Nodes).
