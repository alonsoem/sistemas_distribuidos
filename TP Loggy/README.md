## Comentarios sobre el TP Loggy

### Preparacion del servidor

Iniciamos el emulador de Erlang
```
erl
```

#### Compilamos los modulos:
```
c(time).

c(logger1).

c(worker).

c(test).

```

#### Iniciamos el Test:

```test:run([sleep], [Jitter]).```

Por ejemplo: Inicia el test con un tiempo de sleep de 600 ms y jitter de 100 ms para los workers ``` test:run(600,100). ``` 

Donde:

SLEEP es el tiempo maximo en milisegundos que espera recibir un mensaje antes de intentar el envio de un mensaje.
JITTER es el tiemp maximo en milisegundos que un worker espera desde que puede enviar un mensaje hasta que lo envia.




## Ejercicio en una consola
El logger imprime los mensajes a medida que corresponde hacerlo y en orden:

![Respuesta del logger](./images/corrida.png)

El logger al recibir un mensaje de STOP imprime los mensajes encolados en orden:

![Respuesta del logger](./images/final.png)
