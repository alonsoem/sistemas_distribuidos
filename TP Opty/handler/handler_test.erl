-module(handler_test).
-export([run/0]).

run() ->
  io:format("Running handler tests...~n"),
  test_read(),
  test_read_notfound(),
  test_write(),
  test_commit(),
  test_abort(),
  io:format("All tests completed.~n").

test_read() ->
  io:format("Test: Read operation~n"),
  Client = self(),
  Validator = spawn(fun() -> receive _ -> ok end end),
  Store = store:new(10),
  Handler = handler:start(Client, Validator, Store),

  Handler ! {write, 1, 42},

  Ref = make_ref(),
  Handler ! {read, Ref, 1},
  receive
    {Ref, ok, 42} ->
      io:format("Read test passed.~n");
    Res ->
      io:format("Read test failed.~p~n", [Res])
  after 1000 ->
    io:format("Read test timed out.~n")
  end.

test_read_notfound() ->
  io:format("Test: Read notfound operation~n"),
  Client = self(),
  Validator = spawn(fun() -> receive _ -> ok end end),
  Store = store:new(10),
  Handler = handler:start(Client, Validator, Store),

  Ref = make_ref(),
  Handler ! {read, Ref, 99}, % Leer una clave que no existe
  receive
    { notfound } ->
      io:format("Read notfound test passed.~n");
    Res ->
      io:format("Read notfound test failed.~p~n", [Res])
  after 1000 ->
    io:format("Read notfound test timed out.~n")
  end.

test_write() ->
  io:format("Test: Write operation~n"),
  Client = self(),
  Validator = spawn(fun() -> receive _ -> ok end end),
  Store = store:new(10),
  Handler = handler:start(Client, Validator, Store),
  Handler ! {write, 1, 42},
  Ref = make_ref(),
  Handler ! {read, Ref, 1},
  receive
    {Ref, ok, 42} ->
      io:format("Write test passed.~n");
    _ ->
      io:format("Write test failed.~n")
  after 1000 ->
    io:format("Write test timed out.~n")
  end.

test_commit() ->
  io:format("Test: Commit operation~n"),
  Client = self(),
  Validator = spawn(fun() ->
    receive
      {validate, _Ref, _Reads, _Writes, Client} ->
        Client ! {_Ref, ok}
    end
                    end),
  Store = store:new(10),
  Handler = handler:start(Client, Validator, Store),
  Handler ! {write, 1, 42},
  Ref = make_ref(),
  Handler ! {commit, Ref},
  receive
    {Ref, ok} ->
      io:format("Commit test passed.~n");
    V ->
      io:format("Commit test failed.~p~n", [V])
  after 1000 ->
    io:format("Commit test timed out.~n")
  end.

test_abort() ->
  io:format("Test: Abort operation~n"),
  Client = self(),
  Validator = spawn(fun() -> receive _ -> ok end end),
  Store = store:new(10),
  Handler = handler:start(Client, Validator, Store),
  Handler ! {write, 1, 42},
  Handler ! abort,

  %check if handler process is alive
  case catch is_process_alive(Handler) of
    false ->
      io:format("Test test_abort passed.~n");
    true ->
      io:format("Test test_abort failed.~n")
  end.