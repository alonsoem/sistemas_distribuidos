 -module(worker).
    -export([start/2]).

    -define(timeout, 10000).

    start(Name, Multicaster) ->
        Sleep=rand:uniform(2000),
        
        spawn(fun() -> init(Name, Multicaster, Sleep) end).

    init(Name, Multicaster, Sleep)->
        Multicaster ! {register, self()},
        hello(Name, Multicaster, Sleep).


    hello(Name,Multicaster, Sleep)->
        receive
            registered->
                send(Name,Multicaster,Sleep),
                Gui = spawn(gui, init, [Name]),
                run(Name, Multicaster, Sleep, Gui)

            after ?timeout ->       
                io:format("Register Failed~n")
        
        end.



    run(Name, Multicaster, Sleep, Gui)->
        
        receive
            {msg, {Id, From}}->
                io:format("~p Recibe ~p~n",[Name, {Id, From}]),
                MyId=self(),
                if 
                    From==MyId ->
                        send(Name,Multicaster,Sleep),
                        Gui ! increment,
                        run(Name, Multicaster, Sleep, Gui);
                    true -> 
                    
                        run(Name, Multicaster, Sleep, Gui)

                end;
                
            
            stop ->
                stop() 
        

    
        
        end.



    send(Name, Multicaster, Sleep)->
        timer:sleep(rand:uniform(Sleep)),
        SendId = make_ref(), 
        Multicaster ! {msg, SendId, self()} ,
        io:format("~p Envio Mensaje ~p~n",[Name,SendId]).

    stop()->ok.