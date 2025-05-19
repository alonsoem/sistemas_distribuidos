 -module(worker).
    -export([start/2]).

    -define(timeout, 10000).

    start(Name, Multicaster) ->
        spawn(fun() -> init(Name, Multicaster) end).

    init(Name,Multicaster)->
        Multicaster ! {register, self()},
        hello(Name, Multicaster).


    hello(Name,Multicaster)->
        receive
            registered->
                send(Name,Multicaster),

                run(Name, Multicaster)

            after ?timeout ->       
                io:format("Register Failed~n")
        
        end.



    run(Name, Multicaster)->
        
        receive
            {msg, {Id, From}}->
                io:format("~p Recibe ~p~n",[Name, {Id, From}]),
                MyId=self(),
                if 
                    From==MyId ->
                        send(Name,Multicaster),
                        run(Name, Multicaster);
                    true -> 
                    
                        run(Name, Multicaster)

                end;
                
            
            stop ->
                stop() 
        

    
        
        end.



    send(Name, Multicaster)->
        SendId = make_ref(), 
        Multicaster ! {msg, SendId, self()} ,
        io:format("~p Envio Mensaje ~p~n",[Name,SendId]).

    stop()->ok.