### ¿Mantiene los workers sincronizados?
Sí, el isis_multicaster implementa un algoritmo de orden total (tipo ISIS), donde todos los mensajes enviados a los workers pasan por un proceso de consenso de orden. Cada mensaje es propuesto, se acuerda un número de secuencia global y solo entonces se entrega a los workers. Esto asegura que todos los workers reciben los mensajes en el mismo orden, manteniéndolos sincronizados.


### ¿Cuántos mensajes podemos hacer multicast por segundo y cómo depende esto del número de workers?
La cantidad de mensajes multicast por segundo depende principalmente de:


- El número de multicaster (no de workers): cada mensaje requiere consenso entre todos los multicaster, lo que implica múltiples rondas de mensajes.
- El número de workers no afecta directamente el throughput, ya que los workers solo reciben mensajes ya acordados.
El cuello de botella está en la latencia de la red y el procesamiento de los multicaster, no en la cantidad de workers.
A mayor cantidad de multicaster, más mensajes de coordinación se requieren, lo que puede reducir la cantidad de mensajes por segundo que el sistema puede procesar.