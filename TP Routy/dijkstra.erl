-module(dijkstra).
-export([update/4,entry/2,replace/4,table/2,route/2,iterate/3]).

% update(Node, N, Gateway, Sorted): actualiza la lista Sorted con la
% información de que Node puede ser alcanzado en N saltos usando Gateway.
% Si no se encuentra una entrada, no se agrega la nueva entrada. Solo si
% tenemos un mejor camino (más corto) reemplazamos la entrada existente.



  % tomar la primer entrada de la lista ordenada,
  %encontrar los nodos en el mapa que son alcanzables desde dicha
  %entrada y por cada nodo actualizar la lista ordenada. La entrada
  %tomada de la lista ordenada se agrega a la tabla de ruteo.


entry(Node, Sorted)->
   case lists:keyfind(Node, 1, Sorted) of
    false ->
        0;

    {NodeTo, Length, NodeFrom} ->
      Length

   end.

replace(Node, N, Gateway, Sorted)->
  List = lists:keydelete(Node,1,Sorted),
  UpdatedList = [{Node,N,Gateway} | List],
  %agregar funcion lambda para ordenar por N
  SortedList = lists:keysort(2, UpdatedList),
  SortedList.


update(Node, N,Gateway, Sorted) ->

  % Verifica si ya existe una entrada para el nodo
  case lists:keyfind(Node, 1, Sorted) of
    false ->
      % Si no existe, agrega la nueva entrada
      Sorted;
    {Node, OldN, OldGateway} ->
      % Si existe, verifica si el nuevo camino es mejor
      if
        N < OldN ->
          % Si el nuevo camino es más corto, actualiza la entrada
          lists:keystore(Node, 1, Sorted, {Node, N, Gateway});
        true ->
          % Si no es mejor, deja la lista sin cambios
          Sorted
      end
  end.
  


iterate(Sorted, Map, Table)->
  case Sorted of
      [{_, inf, _} | _] -> Table;
      [{Node, N, Gateway} | Tail]->
                               ReachableList = map:reachable(Gateway,Map),
                               NewSorted= mutarSorted(Tail,ReachableList,N,Map,Gateway),
                               iterate(NewSorted, Map, lists:append([[{Node,Gateway}],Table]));
      []->Table
  end.

    

mutarSorted(Sorted, Nodes, N, Map, Gateway)->
 
  case Nodes of
    [Node|Tail]->
              Length= entry(Node,Sorted),
    
              if
                  Length > N+1 -> 
                      NewSorted=replace(Node, N+1, Gateway, Sorted),
                      mutarSorted(NewSorted,Tail,N,Map,Gateway);
                  true-> 
                      mutarSorted(Sorted,Tail,N,Map,Gateway)
              end;
                  
    []->Sorted
  end.




route(Node,Table)->
  case lists:keyfind(Node,1,Table) of
    {To,Gateway} -> {ok,Gateway};
    false -> notfound
  end.
  


table(Gateways,Map)->
  Nodes = map:all_nodes(Map),
  NotSorted= lists:map(fun(Gateway)-> 
              case lists:member(Gateway, Gateways) of
                true -> {Gateway,0,Gateway};
                false -> {Gateway,inf,unknown}
              end
            end
          ,Nodes),

  Sorted = lists:sort(fun({_,N1,_}, {_,N2,_}) -> N1 =< N2 end, NotSorted),
          
          iterate(Sorted,Map,[]).
          