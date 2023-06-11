-module(common).
-export([llama_tienda/1]).

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
    after 5000 ->
        no
    end.
