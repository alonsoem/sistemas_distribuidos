-module(validator).
-export([start/1]).

start(Store) ->
  spawn_link(fun() -> init(Store) end).

init(Store) ->
  validator(Store).

validator(Store) ->
  receive
    {validate, Ref, Reads, Writes, Client} ->
      case validate(Reads) of
        ok ->
          update(Writes, Store),
          Client ! {Ref, ok};
        abort ->
          Client ! {Ref, abort}
      end,
      validator(Store);
    _Old ->
      validator(Store)
  end.
validate(Reads) ->
  {N, Tag} = send_checks(Reads),
  check_reads(N, Tag).

send_checks(Reads) ->
  Tag = make_ref(),
  Self = self(),
  N = length(Reads),
  lists:map(fun({Entry, Time}) ->
    Entry ! {check, Tag, Time, Self}
            end,
    Reads),
  {N, Tag}.

check_reads(N, Tag) ->
  if
    N == 0 ->
      ok;
    true ->
      receive
        {Tag, ok} ->
          check_reads(N - 1, Tag);
        {Tag, abort} ->
          abort
      end
  end.


update(Writes, Store) ->
  lists:foreach(fun({Index, Value, _Timestamp}) ->
    element(Index, Store) ! {write, Value}
                end, Writes).