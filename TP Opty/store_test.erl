-module(store_test).
-export([run/0]).

run() ->
  io:format("Running tests for store module...~n"),
  test_new(),
  test_lookup(),
  test_stop(),
  io:format("All tests completed.~n").

test_new() ->
  Store = store:new(3),
  case tuple_size(Store) of
    3 ->
      io:format("Test test_new passed.~n");
    _ ->
      io:format("Test test_new failed.~n")
  end.

test_lookup() ->
  Store = store:new(3),
  io:format("Store: ~p~n", [Store]),
  First = element(1, Store),
  case catch store:lookup(1, Store) of
    First ->
      io:format("Test test_lookup passed.~p~n", [First]);
    RES ->
      io:format("Test test_lookup failed.~p~n", [RES])
  end.

test_stop() ->
  Store = store:new(1),
  First = store:lookup(1, Store),
  store:stop(Store),
  case catch is_process_alive(First) of
    false ->
      io:format("Test test_stop passed.~n");
    true ->
      io:format("Test test_stop failed.~n")
  end.