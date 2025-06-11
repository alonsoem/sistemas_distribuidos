-module(test).
-export([run/0,runArg/0,runUru/0]).

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

 %ahora el broadcast lo hace siempre que recibe un add

 BuenosAires ! broadcast,
 Cordoba ! broadcast,
 Rosario ! broadcast,

 % Actualizar tablas de ruteo
 BuenosAires ! update,
 Cordoba ! update,
 Rosario ! update,

 timer:sleep(2000),

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




runArg() ->

  % Iniciar tres routers para probar rutas indirectas
  routy:start(a1, buenosaires),
  routy:start(a2, cordoba),
  routy:start(a3, rosario),
  routy:start(a4, mendoza),

  BuenosAires = erlang:whereis(a1),
  Cordoba = erlang:whereis(a2),
  Rosario = erlang:whereis(a3),
  Mendoza = erlang:whereis(a4),

  io:format("BuenosAires PID: ~p~n", [BuenosAires]),
  io:format("Cordoba PID: ~p~n", [Cordoba]),
  io:format("Rosario PID: ~p~n", [Rosario]),
  io:format("Rosario PID: ~p~n", [Mendoza]),

  % Conectar routers en cadena: BA <-> ROS <-> CBA
  BuenosAires ! {add, rosario, {a3,'argentina@192.168.1.41'}},
  Rosario ! {add, buenosaires, {a1,'argentina@192.168.1.41'}},
  Rosario ! {add, cordoba, {a2,'argentina@192.168.1.41'}},
  Cordoba ! {add, rosario, {a3,'argentina@192.168.1.41'}},
  Cordoba ! {add, mendoza, {a4,'argentina@192.168.1.41'}},


  timer:sleep(400),



  % Probar ruteo indirecto: BuenosAires -> BuenosAires
  a1 ! {send, a1,"Mensaje a si mismo"},
  timer:sleep(1000),

  a1 ! {send, a3,"Mensaje a vecino"},
  timer:sleep(1000),
  a1 ! {send, a2,"Mensaje indirecto a cordoba"},


  ok.


runUru() ->

  % Iniciar tres routers para probar rutas indirectas
  routy:start(u1, montevideo),
  routy:start(u2, colonia),
  
  Montevideo = erlang:whereis(u1),
  Colonia = erlang:whereis(u2),
  

  io:format("Montevideo PID: ~p~n", [Montevideo]),
  io:format("Colonia PID: ~p~n", [Colonia]),
  

  % Conectar routers en cadena: BA <-> ROS <-> CBA
  Montevideo ! {add, colonia, {u2,'uruguay@192.168.1.41'}},
  Colonia ! {add, montevideo, {u1,'uruguay@192.168.1.41'}},
  Colonia ! {add, buenosaires, {a1,'argentina@192.168.1.41'}},
  


  timer:sleep(2000),


  % Probar ruteo indirecto: BuenosAires -> BuenosAires
  u2 ! {send, a1, "Mensaje a Argentina"},
  timer:sleep(1000),


  ok.