%% Archivo: TP%20Final/queue_gui.erl
-module(queue_gui).

-export([start/2, init/2]).

-include_lib("wx/include/wx.hrl").

start(Name, QueuePid) ->
  spawn(queue_gui, init, [Name, QueuePid]).

init(Name, QueuePid) ->
  Width = 400,
  Height = 200,
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, Name, [{size, {Width, Height}}]),
  Panel = wxPanel:new(Frame),
  Font = wxFont:new(19, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
  Text = wxStaticText:new(Panel, -1, "Estado de la cola...", [{pos, {20, 20}}, {size, {360, 40}}]),
  wxStaticText:setFont(Text, Font),
  wxFrame:show(Frame),
  %% Pedir el estado inicial
  self() ! update,
  loop(Frame, Text, QueuePid).

loop(Frame, Text, QueuePid) ->
  receive
    update ->
      State = myqueue:get_state(QueuePid),
      Ready = maps:get(ready, State, []),
      Unacked = maps:get(unacked, State, []),
      Consumers = maps:get(consumers, State, []),
      Name = maps:get(name, State, []),
      TextStr = io_lib:format(
        "Cola:~p~n Ready: ~p~nUnacked: ~p~nConsumers: ~p~n
        -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-",
        [Name, Ready, Unacked, Consumers]
      ),
      wxStaticText:setLabel(Text, TextStr),
      %% Actualiza cada 1 segundo
      erlang:send_after(1000, self(), update),
      loop(Frame, Text, QueuePid);

    stop ->
      ok;

    Error ->
      io:format("gui: strange message ~w ~n", [Error]),
      loop(Frame, Text, QueuePid)
  end.
