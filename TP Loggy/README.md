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

SLEEP es el tiempo máximo en milisegundos que espera recibir un mensaje antes de intentar el envío de un mensaje.

JITTER es el tiempo máximo en milisegundos que un worker espera desde que puede enviar un mensaje hasta que lo envía.




## Ejercicios en una consola


Inicialmente los workers envían los mensajes sin numerar con una etiqueta NA. 
Como esto no permite establecer una linea de tiempo entre los mensajes se hizo necesario implementar una etiqueta con el tiempo de Lamport para poder pensar en algun tipo de orden.

Una vez implementado mediante las funciones Zero/0, inc/1, merge/2 y leq/2 se observó que los mensajes se empezaron a visualizar pero desordenadamente producto de las demoras inducidas en los workers para enviar y escuchar los mensajes. Que el logger imprimiera los mensajes en el orden que los recibia tambien era parte del problema.

![Logger imprimiendo los mensajes como llegaron](./images/unordered.png)

Como cada worker puede enviar el mensaje en cualquier momento fue necesario saber en que momento se podría comenzar a imprimir los mensajes.

¿Si tengo 4 workers tengo que esperar 4 mensajes con un mismo timestamp para saber que los puedo imprimir? ¿Puede que reciba 4 mensajes con 4 timestamp distintos? ¿Si es asi se pueden imprimir? ¿Que necesito para entender que un mensaje es seguro de imprimir?

Entonces implementamos un clock que permite mantener actualizado la etiqueta de tiempo que lleva cada worker. Esto no bastó porque para poder imprimir un mensaje es necesario saber que el Logger no recibirá posteriormente otros mensajes con una etiqueta anterior. Por este motivo implementamos una cola de retencion donde mantendremos los mensajes recibidos ordenados.

Entonces nos propusimos esperar los mensajes 0, 1 ,2 ,3 en ese orden. Cuando todos los workers tengan mensajes superiores al 0 sera seguro imprimir los mensajes con timestamp 0 y asi sucesivamente.

Pero eso no fue todo. Podemos recibir un mensaje de STOP y si simplemente Loggy tambien se detienees posible que en la cola de retencion queden mensajes que no son seguros de imprimir para nosotros.

Bueno, esto nos hizo ver que en realidad si los tenemos ordenados ya es seguro imprimirlos puesto que no llegará ningun otro mensaje luego.




El logger imprime los mensajes a medida que corresponde hacerlo y en orden:

![Respuesta del logger](./images/corrida.png)

El logger al recibir un mensaje de STOP imprime los mensajes encolados en orden:

![Respuesta del logger](./images/final.png)
