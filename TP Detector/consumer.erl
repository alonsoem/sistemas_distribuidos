    -module(consumer).
    -export([start/1, stop/0]).

    start(Producer) ->
        Consumer = spawn(fun() -> consumer(0) end),
        register(client, Consumer),
        Producer! {hello, client}.
        

    stop() ->
        client ! {bye}.


    consumer(Ping) ->
        receive
            {ping, N} ->
                if
                    N==Ping ->
                        io:format("PING  ~w~n", [Ping]),
                        consumer(N+1);        
                    Ping > N ->
                        io:format("PING ARRIBA ~w~n", [Ping]),
                        consumer(N+1)
                end;
                
            {bye} ->
                io:format("CONSUMIDOR DETENIDO"),
                ok

            
        end.