-module(isis_multicaster).
-export([start/0]).
-define(timeout, 1000).

start() ->
  Order = 1,
  spawn(fun() -> init(Order, [], [], [], #{}, #{}) end).

init(Order, Multicasters, Workers, Queue, Proposals, Agreements) ->
  receive
  % Registro de workers
    {register, Worker} ->
      io:format("Registrando worker ~p~n", [Worker]),
      Worker ! registered,
      init(Order, Multicasters, [Worker | Workers], Queue, Proposals, Agreements);

  % Registro de otros multicasters
    {registerMulticaster, M} ->
      io:format("Registrando multicaster ~p~n", [M]),
      M ! {registeredMulticaster, self()},
      init(Order, [M | Multicasters], Workers, Queue, Proposals, Agreements);

  % Worker envía mensaje a multicaster
    {msg, Id, From} ->
      Content = {Id, From},
      lists:foreach(fun(M) -> M ! {propose, Id, self(), Content} end, [self() | Multicasters]),
      init(Order, Multicasters, Workers, Queue, Proposals, Agreements#{Id => {Content, From, 1, [Order]}});

  % Recibe propuesta de orden de otro multicaster
    {propose, Id, Orig, Content} ->
      PropNum = Order + 1,
      Orig ! {proposal, Id, PropNum, self()},
      init(PropNum, Multicasters, Workers, Queue, Proposals, Agreements);

  % Recibe propuesta de otro multicaster
  % Recibe una propuesta de orden de otro multicaster
    {proposal, Id, PropNum, FromM} ->
      case Agreements of
        % Si ya existe un acuerdo parcial para este Id
        #{Id := {Content, Orig, Count, Props}} ->
          NewCount = Count + 1, % Incrementa el contador de propuestas recibidas
          NewProps = [PropNum | Props], % Agrega la nueva propuesta a la lista
          Total = length(Multicasters) + 1, % Total de multicasteres (incluyéndose a sí mismo)
          if NewCount == Total ->
            % Si ya se recibieron todas las propuestas necesarias
            FinalNum = lists:max(NewProps), % Elige el mayor número propuesto como orden final
            lists:foreach(fun(M) -> M ! {agreement, Id, FinalNum, Content, Orig} end, [self() | Multicasters]),
            % Envía el acuerdo final a todos los multicasteres
            % Envía el mensaje acordado a todos los workers
            lists:foreach(fun(W) ->
              io:format("Enviando mensaje ~p a worker ~p~n", [Id, W]),
              W ! {msg, Content}
                          end, Workers),
            NewQueue = [{FinalNum, Id, Content, Orig} | Queue], % Agrega el mensaje a la cola ordenada
            init(Order, Multicasters, Workers, NewQueue, Proposals, maps:remove(Id, Agreements));
            true ->
              % Si aún no se han recibido todas las propuestas, actualiza el acuerdo parcial
              init(Order, Multicasters, Workers, Queue, Proposals, Agreements#{Id => {Content, Orig, NewCount, NewProps}})
          end;
        _ ->
          % Si no hay acuerdo parcial, simplemente continúa
          init(Order, Multicasters, Workers, Queue, Proposals, Agreements)
      end;
  % Recibe el acuerdo final de orden
    {agreement, Id, FinalNum, Content, Orig} ->
      io:format("Recibiendo acuerdo de orden ~p de multicaster ~p~n", [FinalNum, Id]),
      NewQueue = [{FinalNum, Id, Content, Orig} | Queue],
      init(Order, Multicasters, Workers, NewQueue, Proposals, Agreements);

    stop ->
      stop

  after ?timeout ->
    % Entrega mensajes en orden total (ya no envía a los workers aquí)
    SortedQueue = lists:sort(fun({N1,_,_,_}, {N2,_,_,_}) -> N1 =< N2 end, Queue),
    case SortedQueue of
      [] -> init(Order, Multicasters, Workers, Queue, Proposals, Agreements);
      [{Num, Id, Content, Orig} | Rest] ->
        if Num == Order + 1 ->
          init(Num, Multicasters, Workers, Rest, Proposals, Agreements);
          true ->
            init(Order, Multicasters, Workers, Queue, Proposals, Agreements)
        end
    end
  end.