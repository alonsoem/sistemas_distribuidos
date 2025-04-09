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

## Ejercicio en una consola
Consumer recibe los pings de Producer:

![Producer enviando PING a consumer](images\ping.png)


Verificamos que Producer se detenga y notifique a Consumer:

![Producer se detiene y notifica a Consumer](images\producerstop.png)


## Ejercicio en dos nodos

Volvemos a hacer el ejercicio pero en dos nodos distintos:

Primero ejecutamos un proucer en un nodo silver y luego de iniciar el consumer cancelamos el primero.

![Inicio producer en un nodo y cancelo](images\killproducer.png)


El consume se inicia y se cancela el producer para ver si captura la caida:

![Consume refleja el manejo del error](images\consumererror.png)
