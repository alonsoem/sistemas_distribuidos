%% interface_tests.erl
-module(interface_tests).
-export([run/0]).

run() ->
  Pid = self(),
  Intf0 = interface:new(),
  io:format("Test new/0: ~p~n", [Intf0 =:= []]),

  Intf1 = interface:add(alice, ref1, Pid, Intf0),
  io:format("Test add/4: ~p~n", [Intf1 =:= [{alice, ref1, Pid}]]),

  Intf2 = interface:add(bob, ref2, Pid, Intf1),
  io:format("Test add/4 (add bob): ~p~n", [Intf2 =:= [{bob, ref2, Pid}, {alice, ref1, Pid}]]),

  Intf3 = interface:add(alice, ref3, Pid, Intf2),
  io:format("Test add/4 (replace alice): ~p~n", [Intf3 =:= [{alice, ref3, Pid}, {bob, ref2, Pid}]]),

  Intf4 = interface:remove(bob, Intf3),
  io:format("Test remove/2: ~p~n", [Intf4 =:= [{alice, ref3, Pid}]]),

  Lookup1 = interface:lookup(alice, Intf4),
  io:format("Test lookup/2 (alice): ~p~n", [Lookup1 =:= {ok, Pid}]),

  Lookup2 = interface:lookup(bob, Intf4),
  io:format("Test lookup/2 (bob): ~p~n", [Lookup2 =:= notfound]),

  Ref1 = interface:ref(alice, Intf4),
  io:format("Test ref/2 (alice): ~p~n", [Ref1 =:= {ok, ref3}]),

  Ref2 = interface:ref(bob, Intf4),
  io:format("Test ref/2 (bob): ~p~n", [Ref2 =:= notfound]),

  Name1 = interface:name(ref3, Intf4),
  io:format("Test name/2 (ref3): ~p~n", [Name1 =:= {ok, alice}]),

  Name2 = interface:name(ref2, Intf4),
  io:format("Test name/2 (ref2): ~p~n", [Name2 =:= notfound]),

  Names = interface:list(Intf4),
  io:format("Test list/1: ~p~n", [Names =:= [alice]]),

  %% Test broadcast/2 (no assertion, just check no crash)
  interface:broadcast(test_msg, Intf4),
  io:format("Test broadcast/2: ok~n").