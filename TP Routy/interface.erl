-module(interface).
-export([]).


new()
%devuelve una conjunto vacío de interfases.


add(Name, Ref, Pid, Intf)
% agrega una nueva entrada a al conjunto de interfases y devuelve el nuevo conjunto.


remove(Name, Intf)
%remueve una entrada dado el nombre de una interfase y devuelve el nuevo conjunto de interfases.


lookup(Name, Intf) 
%busca el identificador de proceso para un nombre dado, retorna {ok, Pid} o notfound.


ref(Name, Intf)
% busca una referencia para un nombre dado y devuelve {ok, Ref} o notfound.


name(Ref, Intf)
%busca el nombre de una entrada dada una referencia, devuelve {ok, Name} o notfound.


list(Intf): retorna la lista de todos los nombres.


broadcast(Message, Intf)