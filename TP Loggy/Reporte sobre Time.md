## Comentarios el modulo TIME

En terminos generales el modulo Time maneja el tiempo lamport para un tiempo dado.

Inicialmente solo resolvia el tiempo lamport a considerar dados 2 mensaje con distinto timestamp (siempre elegia el valor mayor)

Con esta implementacion y sin mayor logica en el logger lo unico que conseguiamos es que se registre correctamente el mensaje en el logger pero no que se registre en orden. El logger los imprimia cuando los recibia.

Time entonces solo se encargaba de resolver el tiempo lamport que deberia enviar un worker que dado un timestamp recibia otro timestamp de otro worker.

Luego con la implementación de clock resolvio el control del orden de los mensajes.

Clock lleva el registro del timestamp de cada worker. Esto nos permite saber en que momento no recibire mas mensajes con un determinado timestamp y en consecuencia poder imprimirlos en orden. o casi, dado que el clock es una herramienta para hacerlo pero luego los mensajes tienen que ser ordenados en una cola. De lo contrario se imprimiran en el mismo orden de llegada lo cual no es correcto.

Entonces, contando con esta estructura solo necesitamos actualizar el timestamp de un worker cuando este ultimo envie un mensaje y exponer una funcion que permita determinar en que momento es posible imprimir los mensajes de un timestamp, entendiendo que ya no habra otro mensaje con el mismo.

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
