-module(tests).
-export([run_tests/0]).
% Execute all tests
run_tests() ->
  read_test(),
  check_test(),
  stop_test(),
  write_test().


read_test() ->
  Entry = entry:new(42),
  Ref = make_ref(),
  Entry ! {read, Ref, self()},
  receive
    {Ref, _EntryPid, Value, _Timestamp} when Value =:= 42 ->
      io:format("read_test passed: Value is ~p~n", [Value]);
    {Ref, _EntryPid, Value, _Timestamp} ->
      io:format("read_test failed: Unexpected response ~p~n", [Value])
  after 1000 ->
    io:format("read_test failed: No response received~n")
  end.


% Prueba para el mensaje write
write_test() ->
  Entry = entry:new(42),
  Entry ! {write, 100},
  Ref = make_ref(),
  Entry ! {read, Ref, self()},
  receive
    {Ref, _EntryPid, Value, _Timestamp} when Value =:= 100 ->
      io:format("write_test passed: Value is ~p~n", [Value]);
    {Ref, _EntryPid, Value, _Timestamp} ->
      io:format("write_test failed: Value is ~p~n", [Value])
  after 1000 ->
    io:format("write_test failed: No response received~n")
  end.


% Prueba para el mensaje check
check_test() ->
  Entry = entry:new(42),
  Ref = make_ref(),
  Entry ! {check, Ref, make_ref(), self()},
  receive
    {Ref, abort} ->
      io:format("check_test passed: Value is ~p~n", [Ref]),
      ok;
    {Ref, ok} ->
      io:format("check_test failed: Value is ~p~n", [Ref])
  after 1000 ->
    io:format("check_test failed: No response received~n")
  end.

% Prueba para el mensaje stop
stop_test() ->
  Entry = entry:new(42),
  Entry ! stop,
  Ref = make_ref(),
  catch Entry ! {read, Ref, self()},
  receive
    _ ->
      io:format("stop_test failed: Shouldn't respond after stop~n")
  after 1000 ->
    ok
  end.