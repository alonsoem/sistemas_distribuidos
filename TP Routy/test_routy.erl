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

  % Conectar routers en cadena: BA <-> CBA <-> ROS
  % Conectar routers en cadena: BA <-> CBA <-> ROS
  BuenosAires ! {add, cordoba, Cordoba},
  Cordoba ! {add, buenosaires, BuenosAires},
  Cordoba ! {add, rosario, Rosario},
  Rosario ! {add, cordoba, Cordoba},

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
        {status, State} -> io:format("Estado de:~n", [])
      after 1000 -> io:format("Timeout esperando estado de ~p~n", [R])
      end
    end,
    [BuenosAires, Cordoba, Rosario]
  ),

  % Probar ruteo indirecto: BuenosAires -> Cordoba
  BuenosAires ! {send, cordoba,"Directo BA a CBA"},

  % Probar ruteo directo: BuenosAires -> Rosario (debe pasar por Cordoba)
  %BuenosAires ! {send, rosario, {msg, "Directo BA a ROS", self()}},

  % Parar routers
  %lists:foreach(fun(R) -> R ! stop end, [BuenosAires, Cordoba, Rosario]),

  ok.