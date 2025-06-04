-module(dijkstra).
-export([update/4,entry/2,replace/4]).

% update(Node, N, Gateway, Sorted): actualiza la lista Sorted con la
% información de que Node puede ser alcanzado en N saltos usando Gateway.
% Si no se encuentra una entrada, no se agrega la nueva entrada. Solo si
% tenemos un mejor camino (más corto) reemplazamos la entrada existente.

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
  
  SortedList = lists:sort(UpdatedList),
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