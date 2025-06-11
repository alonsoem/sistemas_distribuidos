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
      Map1 = map:update(Node, [Name], Map),
      Table1 = dijkstra:table(interface:list(Intf1), Map1),

      self()!broadcast,
      router(Name, N, Hist, Intf1, Table1, Map1);

    {remove, Node} ->
      {ok, Ref} = interface:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = interface:remove(Node, Intf),
      Table1 = dijkstra:table(interface:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table1, Map);

  %% Maneja la caída de un proceso enlazado (DOWN)
    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = interface:name(Ref, Intf),
      io:format("~w: exit received from ~w~n", [Name, Down]),
      Intf1 = interface:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

  %% Envía el estado actual del router a quien lo solicita
    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

    %% El mensaje es para este router, se imprime y se espera otro mensaje
    {route, Name, _From, Message} ->
      io:format("~p: received message ~p ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);

  %% Rutea un mensaje hacia el destino usando la tabla de ruteo
    {route, To, From, Message} ->
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case interface:lookup(Gw, Intf) of
            {ok, Pid} ->
              io:format("~p: routing message ~p to ~p via ~p~n", [Name, Message, Pid, Gw]),
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          io:format("~p: no route to ~p~n", [Name, To]),
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    {links, Node, R, Links} ->
      case history:update(Node, R, Hist) of
        {new, Hist1} ->
          io:format("~w: new link from ~p to ~p with links ~p~n", [Name, Node, R, Links]),
          interface:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          %io:format("~p map update ~p ~p  ~n", [Name,Map1,Table]),
          %io:format("~p interface ~p  ~n", [Name,interface:list(Intf)]),
          self() ! update,

          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N,Hist, Intf, Table, Map)
      end;

    broadcast ->
      Message = {links, Name, N, interface:list(Intf)},
      interface:broadcast(Message, Intf),
      %%io:format("~w: broadcast ~p ~p~n", [Name, Intf, Map]),
      router(Name, N+1, Hist, Intf, Table, Map);

    update ->
      Table1 = dijkstra:table(interface:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);

    table ->
      io:format("~p Table:  ~p ~n", [Name, Table]),
      router(Name, N, Hist, Intf, Table, Map);
    interface ->
        io:format("~p Interface:  ~p ~n", [Name, interface:list(Intf)]),
        
        router(Name, N, Hist, Intf, Table, Map);
    map ->
      io:format("~p MAP:  ~p ~n", [Name, Map]),
      router(Name, N, Hist, Intf, Table, Map);
  %% Detiene el router
    stop ->
      ok
  end.
