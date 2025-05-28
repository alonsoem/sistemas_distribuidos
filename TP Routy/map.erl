-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() -> [].


% actualiza el mapa para reflejar que un nodo tiene conexiones
%dirigidas a todos los nodos de la lista de links. La entrada vieja
%se elimina.

%> map:update(berlin, [london, paris], []).
%[{berlin,[london,paris]}]
update(Node, Links, Map) ->
  [{Node, Links} | lists:filter(fun({N, _}) -> N =/= Node end, Map)].

%reachable(Node, Map): retorna la lista de nodos directamente alcanzables desde un Node.
reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    {Node, Links} -> Links;
    false -> []
  end.

%retorna la lista de todos los nodos del mapa, también aquellos sin
%conexiones de salida
all_nodes(Map) ->
  lists:foldl(fun({Node, Links}, Rest) -> lists:append([[Node], Rest, Links]) end, [], Map).