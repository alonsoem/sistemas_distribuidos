-module(logger1).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).
    

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    %Inicializo los nodos en 0 (reloj)
    %NewNodes = lists:map(fun(A)->{A,0} end,Nodes),
    NewNodes=time:clock(Nodes),

    io:format("Nodes: ~w ~n", [NewNodes]),
    loop(NewNodes,[],0).


loop(Nodes,Messages,SafeTime) ->
    
    receive
        {log, From, Time, Msg} ->
            UpdatedNodes=time:update (From,Time,Nodes),
            UpdatedMessages = mergeMsg(Messages,From,Time,Msg),
            UpdatedSortedMessages= lists:keysort(2, UpdatedMessages),
            
            case time:safe(SafeTime,Nodes) of
                true -> 
                    PendingMessages = print(UpdatedSortedMessages,SafeTime),
                    loop(UpdatedNodes,PendingMessages,SafeTime+1);

                false -> 
                    loop(UpdatedNodes,UpdatedSortedMessages,SafeTime)
            end;

            
        stop ->
            io:format("RECIBI STOP  ~n"),
            %io:format("RECIBI STOP ~w ~w ~n",[SafeTime,Messages]),
            printPending(Messages,SafeTime)

    end.
    

printPending(Messages,SafeTime)->
    %Imprime los mensajes pendientes en la cola Messages
    case Messages==[] of
        true->
            io:format("NO HAY MAS MENSAJES");
        false -> 
            PendingMessages=print(Messages,SafeTime),
            if 
                Messages==PendingMessages ->
                    io:format("FINAL: ~w ~n", [PendingMessages]);
                true -> 
                    printPending(PendingMessages,SafeTime+1)
            end

    end.


log(From, Time, Msg) ->
    %imprime mensaje
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).


mergeMsg(Messages, From,Time,Msg)->
    %Agrega el mensage a la cola Messages
    [{From,Time,Msg} | Messages].


print(Messages,Time)->
    %Imprime los mensajes que se pueden imprimir y retorna la cola de mensajes sin aquellos impresos.
    FilteredMessages = lists:keyfind(Time, 2, Messages),
    case FilteredMessages of
        false -> Messages;
        _ ->     
            {From,Time,Msg}=FilteredMessages,
            log(From, Time, Msg),
            
            UpdatedMessages=lists:keydelete(Time,2,Messages),
            print(UpdatedMessages,Time)
            
    end.
    