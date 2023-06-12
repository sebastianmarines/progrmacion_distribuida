-module(servidor_tienda).
-export([
    abre_tienda/0,
    servidor/1,
    cierra_tienda/0,
    lista_socios/0,
    probar/0,
    servidor_producto/1,
    registra_producto/2,
    llama_producto/2,
    llenar_datos/0,
    modifica_producto/2
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
            % Mostrar error si la cantidad es menor a 0
            case Cantidad < 0 of
                true ->
                    De ! {servidor_tienda, {error, "Cantidad no puede ser menor a 0"}},
                    servidor(Datos);
                false ->
                    De ! {servidor_tienda, ok},
                    PID = crear_servidor_producto(Cantidad),
                    io:format("PID: ~p~n", [PID]),
                    erlang:monitor(process, PID),
                    servidor(
                        {
                            element(1, Datos),
                            [
                                {Producto, PID} | element(2, Datos)
                            ],
                            element(3, Datos)
                        }
                    )
            end;
        {De, {elimina_producto, Producto}} ->
            io:format("Eliminando producto ~p~n", [Producto]),
            De ! {servidor_tienda, ok},
            servidor(
                {
                    element(1, Datos),
                    lists:keydelete(Producto, 1, element(2, Datos)),
                    element(3, Datos)
                }
            );
        {De, {modificar_producto, Producto, Cantidad}} ->
            io:format("Modificando producto ~p~n", [Producto]),
            % Mostrar error si la cantidad es menor a 0
            case Cantidad < 0 of
                true ->
                    De ! {servidor_tienda, {error, "Cantidad no puede ser menor a 0"}},
                    servidor(Datos);
                false ->
                    De ! {servidor_tienda, ok},
                    {_, PID} = lists:keyfind(Producto, 1, element(2, Datos)),
                    PID ! {self(), {actualizar_disponibilidad, Cantidad}},
                    receive
                        {servidor_producto, ok} ->
                            servidor(
                                {
                                    element(1, Datos),
                                    lists:keyreplace(
                                        Producto, 1, element(2, Datos), {Producto, PID}
                                    ),
                                    element(3, Datos)
                                }
                            )
                    end
            end;
        {De, lista_productos} ->
            io:format("Listando productos~n"),
            De ! {servidor_tienda, element(2, Datos)},
            servidor(Datos);
        {'DOWN', _, process, PID, _} ->
            Producto = lists:keyfind(PID, 2, element(2, Datos)),
            io:format("Eliminando producto ~p~n", [Producto]),
            servidor(
                {
                    element(1, Datos),
                    lists:keydelete(PID, 2, element(2, Datos)),
                    element(3, Datos)
                }
            );
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

elimina_producto(Producto) ->
    llama_tienda({elimina_producto, Producto}).

modifica_producto(Producto, Cantidad) ->
    llama_tienda({modificar_producto, Producto, Cantidad}).

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
    %Eliminar manzanas
    io:format("~p~n", [elimina_producto(manzanas)]),
    % Lista los productos
    io:format("~p~n", [llama_tienda(lista_productos)]),
    % Registramos un producto con cantidad negativa
    io:format("~p~n", [registra_producto(naranjas, -1)]),
    % Elminar producto que no existe
    io:format("~p~n", [elimina_producto(naranjas)]),
    % Modifica peras
    io:format("~p~n", [modifica_producto(peras, 30)]),
    % Listar peras
    io:format("~p~n", [llama_producto(peras, mostrar_disponibilidad)]),
    % Listar productos [{producto, pid}] y llamar mostrar_disponibilidad en cada una
    Productos = llama_tienda(lista_productos),
    lists:foreach(
        fun({Producto, PID}) ->
            io:format("~p~n", [llama_tienda({{PID, Producto}, mostrar_disponibilidad})])
        end,
        Productos
    ),
    % Elimina suscripción de sebastian
    io:format("~p~n", [llama_tienda({eliminar_suscripcion, sebastian})]),
    % Lista los socios
    io:format("~p~n", [lista_socios()]),
    cierra_tienda().

llenar_datos() ->
    % Abre la tienda
    abre_tienda(),
    % Suscribe a sebastian
    io:format("~p~n", [llama_tienda({suscribir, sebastian})]),
    % Registrar manzanas
    io:format("~p~n", [registra_producto(manzanas, 10)]),
    % Registrar peras
    io:format("~p~n", [registra_producto(peras, 20)]).

servidor_producto(Cantidad) ->
    receive
        {De, mostrar_disponibilidad} ->
            io:format("Mostrando disponibilidad~n"),
            De ! {servidor_producto, Cantidad},
            servidor_producto(Cantidad);
        {De, {actualizar_disponibilidad, CantidadNueva}} ->
            io:format("Actualizando disponibilidad~n"),
            De ! {servidor_producto, ok},
            servidor_producto(CantidadNueva);
        {De, _} ->
            De ! {servidor_producto, {error, "Mensaje no reconocido"}},
            servidor_producto(Cantidad)
    end.

crear_servidor_producto(Cantidad) ->
    spawn(servidor_tienda, servidor_producto, [{Cantidad}]).

llama_producto(Producto, Mensaje) ->
    case Producto of
        {_, PID} ->
            monitor(process, PID),
            PID ! {self(), Mensaje},
            receive
                {servidor_producto, Respuesta} ->
                    Respuesta;
                {'DOWN', _, process, PID, _} ->
                    no
            after 5000 ->
                no
            end;
        Producto ->
            {_, PID} = lists:keyfind(Producto, 1, llama_tienda(lista_productos)),
            monitor(process, PID),
            PID ! {self(), Mensaje},
            receive
                {servidor_producto, Respuesta} ->
                    Respuesta;
                {'DOWN', _, process, PID, _} ->
                    no
            after 5000 ->
                no
            end
    end.
