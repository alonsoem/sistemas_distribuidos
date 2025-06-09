-module(interface).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2, names/1]).

% Crea una nueva interfaz vacía.
new() ->
  [].

% Agrega un nuevo elemento (Name, Ref, Pid) a la interfaz, reemplazando si el nombre ya existe.
add(Name, Ref, Pid, Intf) ->
  [{Name, Ref, Pid} | lists:filter(fun({N, _, _}) -> N =/= Name end, Intf)].

% Elimina el elemento con el nombre dado de la interfaz.
remove(Name, Intf) ->
  lists:filter(fun({N, _, _}) -> N =/= Name end, Intf).

% Busca el proceso (Pid) asociado a un nombre en la interfaz.
lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, _, Pid} -> {ok, Pid};
    false -> notfound
  end.

% Busca la referencia (Ref) asociada a un nombre en la interfaz.
ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, Ref, _} -> {ok, Ref};
    false -> notfound
  end.

% Busca el nombre asociado a una referencia (Ref) en la interfaz.
name(Ref, Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    {Name, Ref, _} -> {ok, Name};
    false -> notfound
  end.

% Devuelve la lista de nombres presentes en la interfaz.
list(Intf) ->
  [Name || {Name, _, _} <- Intf]. % Esto es como un map que extrae los nombres de la lista de tuplas.

% Envía un mensaje a todos los procesos (Pids) en la interfaz.
broadcast(Message, Intf) ->
  [Pid ! Message || {_, _, Pid} <- Intf],
  ok.