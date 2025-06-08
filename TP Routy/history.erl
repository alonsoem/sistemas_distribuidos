-module(history).
-export([new/1, update/3]).

% new(Name): retorna una nueva historia, donde los mensajes de Name siempre son vistos como viejos.
new(Name) ->
  [{Name, infinity}].

% update(Node, N, History): chequea si el mensaje con número N desde Node es viejo o nuevo,
% si es nuevo devuelve {new, Updated} donde Updated es la historia actualizada.
update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    false ->
      % Primer mensaje de este nodo
      {new, [{Node, N} | History]};
    {Node, LastN} when N > LastN ->
      % Mensaje nuevo, actualiza el número
      {new, lists:keystore(Node, 1, History, {Node, N})};
    _ ->
      % Mensaje viejo o repetido
      old
  end.