-module(exchange).
-export([start/1]).

start(Name) ->
    start(Name, topic).

start(Name, Type) ->
    spawn(fun() -> init(Name, Type, [], []) end).
init(Name, Type, Messages, Bindings)->
    receive
        {msg, Body, RoutingKey} ->
            io:format("Exchange ~p (~p) recibio Mensaje con routing_key ~p: ~p~n", [Name, Type, RoutingKey, Body]),
            case Type of
                fanout ->
                    broadcast_message(Body, Bindings);
                _ ->
                    route_message(RoutingKey, Body, Bindings)
            end,
            init(Name, Type, [Body|Messages], Bindings);
        {last, From} ->
            case Messages of
                [] ->
                    From ! {msg, "NO HAY MENSAJES"};
                [Hd|_] ->
                    From ! {msg, Hd}
            end,
            init(Name, Type, Messages, Bindings);
        {bind, Queue, RoutingKey} ->
            io:format("Exchange ~p (~p): Binding agregado para cola ~p con routing_key ~p~n", [Name, Type, Queue, RoutingKey]),
            NewBindings = [{Queue, RoutingKey} | Bindings],
            init(Name, Type, Messages, NewBindings);
        {unbind, Queue, RoutingKey} ->
            io:format("Exchange ~p (~p): Eliminando binding para cola ~p con routing_key ~p~n", [Name, Type, Queue, RoutingKey]),
            NewBindings = lists:delete({Queue, RoutingKey}, Bindings),
            init(Name, Type, Messages, NewBindings);
        {get_bindings, From} ->
            From ! {bindings, Bindings},
            init(Name, Type, Messages, Bindings)
    end.

route_message(RoutingKey, Body, Bindings) ->
    MatchingBindings = lists:filter(fun({_, RK}) -> RK == RoutingKey end, Bindings),
    [Queue ! {msg, Body, RoutingKey} || {Queue, _} <- MatchingBindings].

broadcast_message(Body, Bindings) ->
    Queues = [Queue || {Queue, _} <- Bindings],
    [Queue ! {msg, Body, none} || Queue <- Queues].