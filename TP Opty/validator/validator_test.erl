-module(validator_test).
-export([run/0]).

run() ->
  io:format("Iniciando pruebas para el módulo validator2...~n"),
  test_validate_empty_reads(),
  test_validate_with_read(),
 %test_update(),
  io:format("Pruebas finalizadas.~n").

test_validate_empty_reads() ->
  io:format("Iniciando test: test_validate_empty_reads~n"),

  % Crear un store con 1 entrada inicializada
  Store = store:new(1),
  Validator = validator:start(Store),

  % Preparar Reads vacío y una escritura básica
  Reads = [],
  Writes = [{1, "value"}],

  % Crear una referencia única para la validación
  Ref = make_ref(),

  % Enviar mensaje de validación al validator
  Validator ! {validate, Ref, Reads, Writes, self()},

  % Recibir y manejar la respuesta del validator
  receive
    {Ref, ok} ->
      io:format("test_validate_empty_reads: PASSED (Validación exitosa con Reads vacío)~n");
    {Ref, abort} ->
      io:format("test_validate_empty_reads: FAILED (Validación fallida con Reads vacío)~n")
  after 5000 ->
    io:format("test_validate_empty_reads: FAILED (No se recibió respuesta del validator)~n")
  end.


test_validate_with_read() ->
  io:format("Iniciando test: test_validate_with_read~n"),

% Crear un store con 1 entrada inicializada
  Store = store:new(1),
  Validator = validator:start(Store),

  Entry1 = entry:new("value"),

% Preparar Reads con una entrada y una escritura básica
  Reads = [{Entry1, 123}],
  Writes = [{Entry1, 123}],

% Crear una referencia única para la validación
  Ref = make_ref(),

% Enviar mensaje de validación al validator
  Validator ! {validate, Ref, Reads, Writes, self()},

% Recibir y manejar la respuesta del validator
  receive
    {Ref, ok} ->
      io:format("test_validate_with_read: PASSED (Validación exitosa con Reads y Writes)~n");
    {Ref, abort} ->
      io:format("test_validate_with_read: FAILED (Validación fallida con Reads y Writes)~n")
  after 5000 ->
    io:format("test_validate_with_read: FAILED (No se recibió respuesta del validator)~n")
  end.