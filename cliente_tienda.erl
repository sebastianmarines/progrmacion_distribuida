-module(cliente_tienda).
-export([suscribe/1, unsuscribe/1]).

tienda() -> 'servidor@MacBook-Pro-de-Sebastian'.

llama_tienda(Mensaje) ->
    Tienda = tienda(),
    monitor_node(Tienda, true),
    {servidor_tienda, Tienda} ! {self(), Mensaje},
    receive
        {servidor_tienda, Respuesta} ->
            monitor_node(Tienda, false),
            Respuesta;
        {nodedown, Tienda} ->
            no
    end.

suscribe(Cliente) ->
    llama_tienda({suscribir, Cliente}).

unsuscribe(Cliente) ->
    llama_tienda({eliminar_suscripcion, Cliente}).
