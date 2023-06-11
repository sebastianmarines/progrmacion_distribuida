-module(servidor_tienda).
-export([
    abre_tienda/0,
    servidor/1,
    cierra_tienda/0,
    lista_socios/0,
    probar/0,
    servidor_producto/1,
    registra_producto/2
]).
-compile(common).
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
                    servidor({[Quien | element(1, Datos)], element(2, Datos), element(3, Datos)})
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
                    servidor({
                        lists:delete(Quien, element(1, Datos)), element(2, Datos), element(3, Datos)
                    })
            end,
            De ! {servidor_tienda, ok},
            servidor(Datos);
        {De, lista_socios} ->
            io:format("Listando socios~n"),
            De ! {servidor_tienda, element(1, Datos)},
            servidor(Datos);
        {De, {registra_producto, Producto, Cantidad}} ->
            io:format("Registrando producto ~p~n", [Producto]),
            De ! {servidor_tienda, ok},
            servidor(
                {
                    element(1, Datos),
                    [
                        {Producto, crear_servidor_producto(Cantidad)} | element(2, Datos)
                    ],
                    element(3, Datos)
                }
            );
        {De, lista_productos} ->
            io:format("Listando productos~n"),
            De ! {servidor_tienda, element(2, Datos)},
            servidor(Datos);
        {De, _} ->
            De ! {servidor_tienda, {error, "Mensaje no reconocido"}},
            servidor(Datos)
    end.

abre_tienda() ->
    register(
        servidor_tienda,
        spawn(servidor_tienda, servidor, [{[], [], 0}])
    ).

cierra_tienda() ->
    unregister(servidor_tienda).

lista_socios() ->
    llama_tienda(lista_socios).

registra_producto(Producto, Cantidad) ->
    llama_tienda({registra_producto, Producto, Cantidad}).

probar() ->
    % Abre la tienda
    abre_tienda(),
    % Suscribe a sebastian
    io:format("~p~n", [llama_tienda({suscribir, sebastian})]),
    % Lista los socios
    io:format("~p~n", [lista_socios()]),
    % Registrar manzanas
    io:format("~p~n", [registra_producto(manzanas, 10)]),
    % Registrar peras
    io:format("~p~n", [registra_producto(peras, 20)]),
    % Lista los productos
    io:format("~p~n", [llama_tienda(lista_productos)]),
    % Elimina suscripción de sebastian
    io:format("~p~n", [llama_tienda({eliminar_suscripcion, sebastian})]),
    % Lista los socios
    io:format("~p~n", [lista_socios()]),
    cierra_tienda().

servidor_producto(Cantidad) ->
    receive
        {De, mostrar_disponibilidad} ->
            De ! {servidor_producto, Cantidad},
            servidor_producto(Cantidad);
        {De, {actualizar_disponibilidad, CantidadNueva}} ->
            De ! {servidor_producto, ok},
            servidor_producto(CantidadNueva);
        {De, _} ->
            De ! {servidor_producto, {error, "Mensaje no reconocido"}},
            servidor_producto(Cantidad)
    end.

crear_servidor_producto(Cantidad) ->
    spawn(servidor_tienda, servidor_producto, [{Cantidad}]).
