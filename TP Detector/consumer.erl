    -module(consumer).
    -export([start/1, stop/0]).

    start(Producer) ->
        Consumer = spawn(fun() -> init(Producer,0) end),
        register(client, Consumer).
        %Monitor = monitor(process, Producer),
        %Producer ! {hello, self()},
        %consumer(0, Monitor).

        

    stop() ->
        client ! {bye}.

    init(Producer,Ping)->
        Monitor = monitor(process, Producer),
        Producer ! {hello, self()},
        consumer(Ping, Monitor).

    consumer(Ping,Monitor) ->
        receive
            {ping, N} ->
                if
                    N==Ping ->
                        io:format("PING  ~w~n", [Ping]),
                        consumer(N+1,Monitor);        
                    Ping > N ->
                        io:format("PING MAYOR A ~w~n", [Ping]),
                        consumer(N+1,Monitor)
                end;
                
            {'DOWN', Monitor, process, Object, Info} ->
                io:format("~w died; ~w~n", [Object, Info]),
                consumer(Ping, Monitor);

            {bye} ->
                io:format("CONSUMER STOPED"),
                ok

            
        end.