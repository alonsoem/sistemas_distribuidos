-module(myqueue).
-export([start/1, start/2, get_state/1]).

-define(DEFAULT_TTL, 3000).

get_state(Pid) ->
  Pid ! {get_state, self()},
  receive
    {state, State} -> State
  after 1000 ->
    timeout
  end.

start(Name) ->
  start(Name, ?DEFAULT_TTL).

start(Name, TTL) ->
  spawn(fun() -> init(Name, [], [], [], TTL) end).

init(Name, Ready, Unacked, Consumers, DefaultTTL) ->
  receive
    {subscribe, From} ->
      From ! acksubsc,
      case Ready of
        [] ->
          init(Name, Ready, Unacked, [From | Consumers], DefaultTTL);
        [MsgTuple | Tail] ->
          case is_expired(MsgTuple) of
            true ->
              io:format("Mensaje ~p expirado y eliminado~n", [MsgTuple]),
              init(Name, Tail, Unacked, [From | Consumers], DefaultTTL);
            false ->
              {Body, Expiry} = MsgTuple,
              From ! {msg, Body},
              init(Name, Tail, [{From, MsgTuple} | Unacked], [From | Consumers], DefaultTTL)
          end
      end;

    {msg, Body, _RoutingKey} ->
      Expiry = erlang:monotonic_time(millisecond) + DefaultTTL,
      MsgTuple = {Body, Expiry},
      io:format("Queue ~p recibio Mensaje ~w con TTL ~p~n", [Name, Body, DefaultTTL]),
      case Consumers of
        [] ->
          init(Name, [MsgTuple | Ready], Unacked, Consumers, DefaultTTL);
        [Head | Tail] ->
          case is_expired(MsgTuple) of
            true ->
              io:format("Mensaje ~p expirado y eliminado~n", [Body]);
            false ->
              Head ! {msg, Body},
              init(Name, Ready, [{Head, MsgTuple} | Unacked], Tail, DefaultTTL)
          end,
          init(Name, Ready, Unacked, Tail, DefaultTTL)
      end;

    {ack, Msg} ->
      NewUnacked = lists:filter(fun({_, {M, _}}) -> M =/= Msg end, Unacked),
      io:format("Mensaje ~p ACKed y eliminado~n", [Msg]),
      init(Name, Ready, NewUnacked, Consumers, DefaultTTL);

    {nack, Msg} ->
      % Buscar el consumidor y mensaje en Unacked, eliminarlo y devolver a Ready si no expiró
      {Found, RestUnacked} = lists:partition(fun({_, {M, _}}) -> M =:= Msg end, Unacked),
      case Found of
        [{_, MsgTuple}] ->
          case is_expired(MsgTuple) of
            true ->
              io:format("Mensaje ~p NACKed pero expirado, eliminado~n", [Msg]);
            false ->
              io:format("Mensaje ~p NACKed, vuelve a la cola~n", [Msg]),
              init(Name, [MsgTuple | Ready], RestUnacked, Consumers, DefaultTTL)
          end,
          init(Name, Ready, RestUnacked, Consumers, DefaultTTL);
        _ ->
          init(Name, Ready, Unacked, Consumers, DefaultTTL)
      end;
    {get_state, From} ->
      State = #{ready => Ready, unacked => Unacked, consumers => Consumers, name => Name},
      From ! {state, State},
      init(Name, Ready, Unacked, Consumers, DefaultTTL)
  end.

is_expired({_, Expiry}) ->
  erlang:monotonic_time(millisecond) > Expiry.

get_ttl(none, Default) -> Default;
get_ttl(Opts, Default) when is_list(Opts) ->
  case lists:keyfind(ttl, 1, Opts) of
    {ttl, T} -> T;
    _ -> Default
  end;
get_ttl(_, Default) -> Default.
