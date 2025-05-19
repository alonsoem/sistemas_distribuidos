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
                wait(Multicaster)

            after ?timeout ->       
                io:format("Register Failed~n")
        
        end.

    wait(Multicaster)->
        io:format("waiting~n"),
        receive
            {received}->
                wait(Multicaster);

            stop ->
                stop() 
        
        end.




    stop()->ok.