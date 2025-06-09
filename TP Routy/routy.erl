-module(routy).
-export([start/2, stop/1]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = interface:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = history:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Intf1 = interface:add(Node, Ref, Pid, Intf),
      Table1 = dijkstra:table(interface:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table1, Map);

    {remove, Node} ->
      {ok, Ref} = interface:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = interface:remove(Node, Intf),
      Table1 = dijkstra:table(interface:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table1, Map);

  %% Maneja la caída de un proceso enlazado (DOWN)
    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = interface:name(Ref, Intf),
      io:format("~w: exit recived from ~w~n", [Name, Down]),
      Intf1 = interface:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

  %% Envía el estado actual del router a quien lo solicita
    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

  %% Inicia el ruteo de un mensaje desde un usuario local
    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    %% El mensaje es para este router, se imprime y se espera otro mensaje
    {route, Name, _From, Message} ->
      io:format("~w: received message ~w ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);

  %% Rutea un mensaje hacia el destino usando la tabla de ruteo
    {route, To, From, Message} ->
      io:format("~w: routing message (~w)", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case interface:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    {links, Node, R, Links} ->
      case history:update(Node, R, Hist) of
        {new, Hist1} ->
          interface:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N, Hist, Intf, Table, Map)
      end;

    broadcast ->
      Message = {links, Name, N, interface:list(Intf)},
      interface:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);

    update ->
      Table1 = dijkstra:table(interface:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);
  %% Detiene el router
    stop ->
      ok
  end.
