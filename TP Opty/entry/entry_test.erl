-module(entry_test).
-export([run/0]).

run() ->
  io:format("Iniciando pruebas del módulo entry...~n"),
  test_new(),
  test_read(),
  test_write(),
  test_check(),
  test_stop(),
  io:format("Pruebas finalizadas.~n").

test_new() ->
  Pid = entry:new(42),
  case is_process_alive(Pid) of
    true -> io:format("test_new: OK~n");
    false -> io:format("test_new: FAIL~n")
  end.

test_read() ->
  Pid = entry:new(42),
  Ref = make_ref(),
  Pid ! {read, Ref, self()},
  receive
    {Ref, Pid, 42, _Time} ->
      io:format("test_read: OK~n");
    _ ->
      io:format("test_read: FAIL~n")
  after 1000 ->
    io:format("test_read: TIMEOUT~n")
  end.

test_write() ->
  Pid = entry:new(42),
  Pid ! {write, 100},
  Ref = make_ref(),
  Pid ! {read, Ref, self()},
  receive
    {Ref, Pid, 100, _Time} ->
      io:format("test_write: OK~n");
    _ ->
      io:format("test_write: FAIL~n")
  after 1000 ->
    io:format("test_write: TIMEOUT~n")
  end.

test_check() ->
  Pid = entry:new(42),
  Ref = make_ref(),
  Pid ! {read, Ref, self()},
  receive
    {Ref, _, _, Time} ->
      Pid ! {check, Ref, Time, self()},
      receive
        {Ref, ok} ->
          io:format("test_check: OK~n");
        {Ref, abort} ->
          io:format("test_check: FAIL (abort)~n")
      after 1000 ->
        io:format("test_check: TIMEOUT~n")
      end
  after 1000 ->
    io:format("test_check: TIMEOUT~n")
  end.

test_stop() ->
  Pid = entry:new(42),
  Pid ! stop,
  timer:sleep(100),
  case is_process_alive(Pid) of
    true -> io:format("test_stop: FAIL~n");
    false -> io:format("test_stop: OK~n")
  end.