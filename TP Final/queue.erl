 -module(queue).
    -export([start/1]).

start(Name) ->
    spawn(fun() -> init(Name, []) end).

init(Name, Messages)->
    receive
        {msg,Body}->
            io:format("Queue ~p recibio Mensaje  ~p~n",[Name,Body]),
            init(Name, [Body|Messages])
    end.