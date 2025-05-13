-module(time).
-export([zero/0,inc/1,merge/2,clock/1,safe/2,update/3]).

    %retorna un valor Lamport inicial (puede ser 0).
    zero() ->
        0.


    %retorna el tiempo T incrementado en uno (probablemente ignoremos el Name, pero lo usaremos más adelante).

    inc(T) ->
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


    clock(Nodes)->
        %crea un clock y lo devuelve
        lists:map(fun(A)->{A,zero()} end,Nodes).
              
        
    update(From, Time, Clock)->
        %retorna un reloj que haya sido actualizado dado que hemos recibido un mensaje de log de un nodo en determinado momento.
        lists:keyreplace(From,1,Clock,{From,Time}).

    
    
    safe (Time, Clock)->
        %retorna True si es seguro imprimir mensajes con Time dado
        lists:all(fun(Node)->element(2, Node) > Time end,Clock).
