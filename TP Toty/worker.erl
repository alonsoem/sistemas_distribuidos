 -module(worker).
    -export([start/2]).

    -define(timeout, 5000).

    start(Name, Multicaster) ->
        spawn(fun() -> init(Multicaster) end).

    init(Multicaster)->
        Multicaster ! {register, self()},
        hello(Multicaster).


    hello(Multicaster)->
        receive
            registered->
                run(Multicaster)

            after ?timeout ->       
                io:format("Register Failed~n")
        
        end.



    run(Multicaster)->
        
        receive
            {msg, Ref}->
                io:format("Receive ~p~n",[Ref]),
                run(Multicaster);

            stop ->
                stop() 
        
        after ?timeout ->     
                Id = make_ref(), 
                Multicaster ! {msg, Id, self()}  ,
                io:format("Envio Mensaje ~n"),
                run(Multicaster)
    
        
        end.




    stop()->ok.