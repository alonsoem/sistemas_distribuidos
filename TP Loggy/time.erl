-module(time).
-export([zero/0,inc/2,merge/2]).

    %retorna un valor Lamport inicial (puede ser 0).
    zero() ->
        0.


    %retorna el tiempo T incrementado en uno (probablemente ignoremos el Name, pero lo usaremos más adelante).

    inc(Name, T) ->
        T+1.


    % unifica los dos timestamps Lamport (eso es, toma el mayor).
    merge(Ti, Tj) ->
        case leq(Ti,Tj) of
            true ->  
                Tj;
            false -> 
                Ti 
        end.

    %retorna true si Ti es menor o igual a Tj.
    leq(Ti, Tj)->
        if 
            Ti>= Tj-> false;
            Ti<Tj ->true
        end.



    %retorna un reloj que pueda llevar cuenta de los nodos
    clock(Nodes)->
        clock:start(Nodes).
        

    %retorna un reloj que haya sido actualizado dado que hemos recibido un mensaje de log de un nodo en determinado momento.
        
    update(Node, Time, Clock)->
        Clock!{update,Node,Time}.

    %retorna true o false si es seguro enviar el mensaje de log de un evento que ocurrió en el tiempo Time dado.
    safe(Time, Clock)->
        ClockTime=Clock:value(),
        if 
            Time==ClockTime -> true;
            Time/=ClockTime -> false
        end.