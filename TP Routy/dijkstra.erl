-module(dijkstra).
-export([update/4,entry/2,replace/4,table/2,recorrerMap/1,route/2]).

% update(Node, N, Gateway, Sorted): actualiza la lista Sorted con la
% información de que Node puede ser alcanzado en N saltos usando Gateway.
% Si no se encuentra una entrada, no se agrega la nueva entrada. Solo si
% tenemos un mejor camino (más corto) reemplazamos la entrada existente.



  % tomar la primer entrada de la lista ordenada,
  %encontrar los nodos en el mapa que son alcanzables desde dicha
  %entrada y por cada nodo actualizar la lista ordenada. La entrada
  %tomada de la lista ordenada se agrega a la tabla de ruteo.
route(Node,Table)->
  case lists:keyfind(Node,1,Table) of
    {To,Gateway} -> {ok,Gateway};
    false -> notfound
  end.
  





table(Gateways, Map) ->
  FilteredMap= lists:filter(fun ({G,_})-> lists:member(G, Gateways) end,Map),
  TupleLists= lists:map(fun(X) -> recorrerMap(X) end , FilteredMap),
  lists:merge(TupleLists).

recorrerMap(GatewayLinks)->
      case GatewayLinks of
          {Gateway, Links} -> getLinks(Gateway,Links)
      end.


getLinks(G,L)->
  case L of
    [H|T] ->[{G,H}| getLinks(G,T)];
    []->[]
  end.


%route(Node, Table)->


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