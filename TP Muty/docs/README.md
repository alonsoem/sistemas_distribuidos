## Lock1

 Notamos que al ejcutar pruebas de muty con el lock1, existen deadlocks y se pierde la garantia dle orden del llegada.
 Algunos procesos pueden nunca obtener el lock, especialmente si hay procesos rápidos que entran y salen constantemente.
 ![img.png](img.png)
Ademas, existe una relacion entre el tiempo de espera y el tiempo de procesamiento,a medida que aumentamos tiempo en la zona critica, los procesoss son mas propensos a deadlocks
## Lock2

Lock2 es una mejora sobre lock1, ya que no tiene deadlocks y mantiene la garantia de orden de llegada.
Sin embargo, no es tan eficiente como el lock1, ya que puede haber un mayor tiempo de espera para obtener el lock.

![img_1.png](img_1.png)

Un dato importante es tener en cuenta que el tiempo de trabajo en la zona critica no sea muy cercano o superior al tiempo de deadlock configurado.
-define(deadlock, 5000). por ejemplo si el tiempo de trabajo es 5000, todos los procesos daran deadlocks por una falla en la definicion de un deadlock.
Tambien aplicamos una mejora para que no esten todos los procesos sin prioridad esperando, sino que se le envia inmediatamente el mensaje defer para que vuelvan a intentar pedir el lock luego de unos milisegundos.
