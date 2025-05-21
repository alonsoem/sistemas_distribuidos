 -module(publisher).
    -export([start/2]).

    -define(timeout, 10000).

    start(Name, Exchange) ->
        Sleep=rand:uniform(2000),
        
        spawn(fun() -> init(Name, Exchange, Sleep) end).

    init(Name, Exchange, Sleep)->
        receive
        
            last->
                Exchange ! {last, self()};
            


            {msg, Body} ->
                io:format("Exchange envio Mensaje  ~p~n",[Body])

        after 2000 ->
            send(Name, Exchange, Sleep)
        end,
        
        init(Name,Exchange,Sleep).

    send(Name, Exchange, Sleep)->
        timer:sleep(rand:uniform(Sleep)),
        
        Exchange ! {msg,  self()}.

    
        

    stop()->ok.