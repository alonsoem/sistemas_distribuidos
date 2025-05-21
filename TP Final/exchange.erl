 -module(exchange).
    -export([start/1]).

start(Name) ->
    spawn(fun() -> init(Name, []) end).

init(Name, Messages)->
    receive
        {msg,Body}->
            io:format("Exchange ~p recibio Mensaje  ~p~n",[Name,Body]),
            init(Name, [Body|Messages]);
        {last, From} ->
            case Messages of
                [] -> 
                    From ! {msg,"NO HAY MENSAJES"};
                [Hd|Tl] -> 
                    From ! {msg,Hd}
            end,
            init (Name,Messages)
             
    end.