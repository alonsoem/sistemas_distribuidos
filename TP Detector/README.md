## Comentarios sobre el TP Detector

### Preparacion del servidor

Iniciamos el emulador de Erlang
```
erl
```

#### Compilamos los modulos:
```
c(producer).

c(consumer).

```

#### Iniciamos el Producer:

```producer:start([timeout]).```

Por ejemplo: Inicia el PRODUCER con un timeout de 2s para enviar Pings ``` producer:start(2000). ``` 


#### Iniciamos el Consumer:

```consumer:start([producer]).```

Por ejemplo: Inicia el CONSUMER con un proceso de producer.  ``` consumer:start(producer). ``` 


### Resultado 

![Producer enviando PING a consumer](ping.png)
Con las lineas de reply 
```
reply({{get, URI, _}, _, _}) ->
    http:ok(...).
```
el resultado fue 

`4> test:bench("192.168.0.3", 8080).`
1362841

y con las lineas de reply   

```reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(...).

```
 `test:bench("192.168.0.3", 8080).`
1031782


sin embargo, al intentarlo varias veces consecutivas el resultado varia mucho


```
5> test:bench("192.168.0.3", 8080).
998503
6> test:bench("192.168.0.3", 8080).
2101863
7> test:bench("192.168.0.3", 8080).
939009
8> test:bench("192.168.0.3", 8080).
3079273
9> test:bench("192.168.0.3", 8080).
939725
```

Probe agregando una linea de spawn a un nuevo proceso en cada request para hacerlas asincronicas,

```
spawn(fun() -> request(Client) end),  % Maneja cada request en un proceso separado
```
 pero el resultado no fue bueno, el tiempo de demora fue mayor, estimo que se debera a que la eficiencia de \
 cantidad de threads va a variar dependiendo de los nucleaos de procesador

```
31> test:bench("192.168.0.3", 8080).
5268893
32> test:bench("192.168.0.3", 8080).
5137001
33> test:bench("192.168.0.3", 8080).
5127786
34> test:bench("192.168.0.3", 8080).
5108022
35> test:bench("192.168.0.3", 8080).
5234896

```
