-module(test_routy).
-export([run/0]).

run() ->
  % Limpiar routers previos
  lists:foreach(
    fun(Name) ->
      case erlang:whereis(Name) of
        undefined -> ok;
        Pid -> exit(Pid, kill)
      end
    end,
    [buenosaires, cordoba, rosario]
  ),

  % Iniciar tres routers para probar rutas indirectas
  routy:start(buenosaires, buenosaires),
  routy:start(cordoba, cordoba),
  routy:start(rosario, rosario),

  BuenosAires = erlang:whereis(buenosaires),
  Cordoba = erlang:whereis(cordoba),
  Rosario = erlang:whereis(rosario),

  io:format("BuenosAires PID: ~p~n", [BuenosAires]),
  io:format("Cordoba PID: ~p~n", [Cordoba]),
  io:format("Rosario PID: ~p~n", [Rosario]),

  % Conectar routers en cadena: BA <-> ROS <-> CBA
  BuenosAires ! {add, rosario, Rosario},
  Rosario ! {add, buenosaires, BuenosAires},
  Rosario ! {add, cordoba, Cordoba},
  Cordoba ! {add, rosario, Rosario},

 BuenosAires ! broadcast,
 Cordoba ! broadcast,
 Rosario ! broadcast,

  % Actualizar tablas de ruteo
  BuenosAires ! update,
  Cordoba ! update,
  Rosario ! update,

  timer:sleep(200),

  % Consultar estado de todos los routers
  lists:foreach(
    fun(R) ->
      R ! {status, self()},
      receive
        {status, {Name, N, Hist, Intf, Table, Map}} -> io:format("Tabla de ~p: ~p~n", [Name,Table])
      after 1000 -> io:format("Timeout esperando estado de ~p~n", [R])
      end
    end,
    [BuenosAires, Cordoba, Rosario]
  ),

  % Probar ruteo indirecto: BuenosAires -> BuenosAires
  BuenosAires ! {send, buenosaires,"Mensaje a si mismo"},
  timer:sleep(1000),
  BuenosAires ! {send, rosario,"Mensaje a vecino"},
  timer:sleep(1000),
  BuenosAires ! {send, cordoba,"Mensaje indirecto a cordoba"},

  ok.