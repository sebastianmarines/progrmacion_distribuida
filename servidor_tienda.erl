-module(servidor_tienda).
-export([abre_tienda/0, servidor/1, cierra_tienda/0, lista_socios/0, probar/0]).
-import(common, [llama_tienda/1]).

servidor(Datos) ->
    receive
        {De, {suscribir, Quien}} ->
            io:format("Suscribiendo a ~p~n", [Quien]),
            % Si Quien ya está suscrito, no lo suscribimos de nuevo y enviamos un mensaje de error
            case lists:member(Quien, element(1, Datos)) of
                true ->
                    De ! {servidor_tienda, {error, Quien}},
                    servidor(Datos);
                false ->
                    De ! {servidor_tienda, ok},
                    servidor({[Quien | element(1, Datos)], element(2, Datos)})
            end,
            De ! {servidor_tienda, ok},
            servidor(Datos);
        {De, {eliminar_suscripcion, Quien}} ->
            % Print datos
            io:format("Eliminando suscripción de ~p~n", [Quien]),
            % Si Quien no está suscrito, no lo eliminamos y enviamos un mensaje de error
            case lists:member(Quien, element(1, Datos)) of
                false ->
                    De ! {servidor_tienda, {error, Quien}},
                    servidor(Datos);
                true ->
                    De ! {servidor_tienda, ok},
                    servidor({lists:delete(Quien, element(1, Datos)), element(2, Datos)})
            end,
            De ! {servidor_tienda, ok},
            servidor(Datos);
        {De, lista_socios} ->
            io:format("Listando socios~n"),
            De ! {servidor_tienda, element(1, Datos)},
            servidor(Datos);
        {De, _} ->
            De ! {servidor_tienda, {error, "Mensaje no reconocido"}},
            servidor(Datos)
    end.

abre_tienda() ->
    register(
        servidor_tienda,
        spawn(servidor_tienda, servidor, [{[], []}])
    ).

cierra_tienda() ->
    unregister(servidor_tienda).

lista_socios() ->
    llama_tienda(lista_socios).

probar() ->
    abre_tienda(),
    io:format("Abriendo tienda~n"),
    io:format("Suscribiendo a sebastian~n"),
    io:format("~p~n", [llama_tienda({suscribir, sebastian})]),
    io:format("Listando socios~n"),
    io:format("~p~n", [lista_socios()]),
    cierra_tienda().
