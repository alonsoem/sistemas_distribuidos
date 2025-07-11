-module(myqueue).
-export([start/1, start/2, start/3, get_state/1]).

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
  start(Name, TTL, none).

start(Name, TTL, DeadLetterQueue) ->
  spawn(fun() -> init(Name, [], [], [], TTL, DeadLetterQueue) end).

init(Name, Ready, Unacked, Consumers, DefaultTTL, DeadLetterQueue) ->
  receive
    {subscribe, From} ->
      From ! acksubsc,
      case Ready of
        [] ->
          init(Name, Ready, Unacked, [From | Consumers], DefaultTTL, DeadLetterQueue);
        [MsgTuple | Tail] ->
          case is_expired(MsgTuple) of
            true ->
              handle_expired(MsgTuple, DeadLetterQueue),
              init(Name, Tail, Unacked, [From | Consumers], DefaultTTL, DeadLetterQueue);
            false ->
              {Body, Expiry} = MsgTuple,
              From ! {msg, Body},
              init(Name, Tail, [{From, MsgTuple} | Unacked], [From | Consumers], DefaultTTL, DeadLetterQueue)
          end
      end;

    {msg, Body, _RoutingKey} ->
      Expiry = erlang:monotonic_time(millisecond) + DefaultTTL,
      MsgTuple = {Body, Expiry},
      io:format("Queue ~p recibio Mensaje ~w con TTL ~p~n", [Name, Body, DefaultTTL]),
      case Consumers of
        [] ->
          init(Name, [MsgTuple | Ready], Unacked, Consumers, DefaultTTL, DeadLetterQueue);
        [Head | Tail] ->
          case is_expired(MsgTuple) of
            true ->
              handle_expired(MsgTuple, DeadLetterQueue);
            false ->
              Head ! {msg, Body},
              init(Name, Ready, [{Head, MsgTuple} | Unacked], Tail, DefaultTTL, DeadLetterQueue)
          end,
          init(Name, Ready, Unacked, Tail, DefaultTTL, DeadLetterQueue)
      end;

    {ack, Msg} ->
      NewUnacked = lists:filter(fun({_, {M, _}}) -> M =/= Msg end, Unacked),
      io:format("Mensaje ~p ACKed y eliminado~n", [Msg]),
      init(Name, Ready, NewUnacked, Consumers, DefaultTTL, DeadLetterQueue);

    {nack, Msg} ->
      {Found, RestUnacked} = lists:partition(fun({_, {M, _}}) -> M =:= Msg end, Unacked),
      case Found of
        [{_, MsgTuple}] ->
          case is_expired(MsgTuple) of
            true ->
              handle_expired(MsgTuple, DeadLetterQueue);
            false ->
              io:format("Mensaje ~p NACKed, vuelve a la cola~n", [Msg]),
              init(Name, [MsgTuple | Ready], RestUnacked, Consumers, DefaultTTL, DeadLetterQueue)
          end,
          init(Name, Ready, RestUnacked, Consumers, DefaultTTL, DeadLetterQueue);
        _ ->
          init(Name, Ready, Unacked, Consumers, DefaultTTL, DeadLetterQueue)
      end;

    {get_state, From} ->
      State = #{ready => Ready, unacked => Unacked, consumers => Consumers, name => Name},
      From ! {state, State},
      init(Name, Ready, Unacked, Consumers, DefaultTTL, DeadLetterQueue)
  end.

handle_expired(MsgTuple, none) ->
  io:format("Mensaje ~p expirado y eliminado~n", [MsgTuple]);
handle_expired({Message,_}, DeadLetterQueue) ->
  DeadLetterQueue ! {msg, Message, none},
  io:format("Mensaje ~w expirado y enviado a Dead Letter Queue~n", [Message]).

is_expired({_, Expiry}) ->
  erlang:monotonic_time(millisecond) > Expiry.

get_ttl(none, Default) -> Default;
get_ttl(Opts, Default) when is_list(Opts) ->
  case lists:keyfind(ttl, 1, Opts) of
    {ttl, T} -> T;
    _ -> Default
  end;
get_ttl(_, Default) -> Default.
