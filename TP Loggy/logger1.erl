-module(logger1).

-export([start/1, stop/1]).

start(Nodes) ->
    NewNodes = lists:map(fun(A)->{A,0} end,Nodes),
    spawn_link(fun() ->init(NewNodes) end).
    

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
%    loop(Nodes).
    %Clock=clock:start(Nodes),
    io:format("Nodes: ~w ~n", [Nodes]),
    loop(Nodes,[],0).



%loop(Nodes) ->
loop(Nodes,Messages,SafeTime) ->
    
    receive
        {log, From, Time, Msg} ->
            %io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
            UpdatedNodes=update (From,Nodes,Time),
            UpdatedMessages = mergeMsg(Messages,From,Time,Msg),
            UpdatedSortedMessages= lists:keysort(2, UpdatedMessages),
            
            case safe(SafeTime,Nodes) of
                true -> 
                    PendingMessages = print(UpdatedSortedMessages,SafeTime),
                    loop(UpdatedNodes,PendingMessages,SafeTime+1);

                false -> loop(UpdatedNodes,UpdatedSortedMessages,SafeTime)
            end;

            
        stop ->
            io:format("ENDING ~w ~w ~n",[SafeTime,Messages]),
            printPending(Messages,SafeTime)
                

                
            

    end.
    

printPending(Messages,SafeTime)->
    case Messages==[] of
        true->
            io:format("FINALLY END");
        false -> 
            io:format("Messages: ~w ~n", [Messages]),
            PendingMessages=print(Messages,SafeTime),
        
            
            if 
                Messages==PendingMessages ->
                    io:format("FINAL: ~w ~n", [PendingMessages]);
                true -> 
                    printPending(PendingMessages,SafeTime+1)
            end
            
    
            
    end.
            



log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

update(Name,Nodes,Time)->
    lists:keyreplace(Name,1,Nodes,{Name,Time}).
	

safe (Time,Nodes)->
    lists:all(fun(Node)->element(2, Node)>Time end,Nodes).

mergeMsg(Messages, From,Time,Msg)->
    [{From,Time,Msg} | Messages].


print(Messages,Time)->
    FilteredMessages = lists:keyfind(Time, 2, Messages),
    case FilteredMessages of
        false -> Messages;
        _ ->     
            {From,Time,Msg}=FilteredMessages,
            log(From, Time, Msg),
            
            UpdatedMessages=lists:keydelete(Time,2,Messages),
            print(UpdatedMessages,Time)
            
    end.
    