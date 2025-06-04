module(history)

new(Name)
% retorna una nueva historia, donde los mensajes de Name siempre son vistos como viejo.

update(Node, N, History)
% chequea si el mensaje con número N desde Node es viejo o nuevo,si es nuevo devuelve {new, Updated} donde Updated es la historia actualizada.