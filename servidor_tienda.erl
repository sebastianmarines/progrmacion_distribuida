% Sebastian Marines Alvarez A01383056
% Saul Sanchez Rangel A01383954

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
    modifica_producto/2,
    productos_vendidos/0,
    elimina_producto/1
]).
-compile(common).
-import(common, [llama_tienda/1]).

servidor(Datos) ->
    % Datos es una tupla con los siguientes elementos:
    % 1. Lista de socios
    % 2. Lista de productos
    % 3. Contador de pedidos
    % 4. Lista de productos vendidos
    receive
        {De, {suscribir, Quien}} ->
            io:format("[Tienda]: Suscribiendo a ~p~n", [Quien]),
            % Si Quien ya está suscrito, no lo suscribimos de nuevo y enviamos un mensaje de error
            case lists:member(Quien, element(1, Datos)) of
                true ->
                    De ! {servidor_tienda, {error, Quien}},
                    servidor(Datos);
                false ->
                    De ! {servidor_tienda, ok},
                    servidor({
                        [Quien | element(1, Datos)],
                        element(2, Datos),
                        element(3, Datos),
                        element(4, Datos)
                    })
            end;
        {De, {eliminar_suscripcion, Quien}} ->
            io:format("[Tienda]: Eliminando suscripción de ~p~n", [Quien]),
            % Si Quien no está suscrito, no lo eliminamos y enviamos un mensaje de error
            case lists:member(Quien, element(1, Datos)) of
                false ->
                    De ! {servidor_tienda, {error, Quien}},
                    servidor(Datos);
                true ->
                    De ! {servidor_tienda, ok},
                    servidor({
                        lists:delete(Quien, element(1, Datos)),
                        element(2, Datos),
                        element(3, Datos),
                        element(4, Datos)
                    })
            end;
        {De, lista_socios} ->
            io:format("[Tienda]: Listando socios~n"),
            De ! {servidor_tienda, element(1, Datos)},
            servidor(Datos);
        {De, {registra_producto, Producto, Cantidad}} ->
            io:format("[Tienda]: Registrando producto ~p~n", [Producto]),
            % Mostrar error si la cantidad es menor a 0
            case Cantidad < 0 of
                true ->
                    De ! {servidor_tienda, {error, "Cantidad no puede ser menor a 0"}},
                    servidor(Datos);
                false ->
                    De ! {servidor_tienda, ok},
                    % Se crea el servidor de producto y almacenamos el PID
                    PID = crear_servidor_producto(Cantidad),
                    % Agregamos un monitor al servidor de producto que mande un mensaje al servidor de tienda cuando se caiga
                    erlang:monitor(process, PID),
                    servidor(
                        {
                            element(1, Datos),
                            [
                                {Producto, PID} | element(2, Datos)
                            ],
                            element(3, Datos),
                            element(4, Datos)
                        }
                    )
            end;
        {De, {elimina_producto, Producto}} ->
            io:format("[Tienda]: Eliminando producto ~p~n", [Producto]),
            De ! {servidor_tienda, ok},
            servidor(
                {
                    element(1, Datos),
                    lists:keydelete(Producto, 1, element(2, Datos)),
                    element(3, Datos)
                }
            );
        {De, {modificar_producto, Producto, Cantidad}} ->
            io:format("[Tienda]: Modificando producto ~p~n", [Producto]),
            % Mostrar error si la cantidad es menor a 0
            case Cantidad < 0 of
                true ->
                    De ! {servidor_tienda, {error, "Cantidad no puede ser menor a 0"}},
                    servidor(Datos);
                false ->
                    De ! {servidor_tienda, ok},
                    % Buscamos el PID del producto
                    {_, PID} = lists:keyfind(Producto, 1, element(2, Datos)),
                    % Mandamos un mensaje al servidor de producto para actualizar la cantidad
                    PID ! {self(), {actualizar_disponibilidad, Cantidad}},
                    receive
                        % Recibimos la respuesta del servidor de producto y actualizamos la lista de productos
                        {servidor_producto, ok} ->
                            servidor(
                                {
                                    element(1, Datos),
                                    lists:keyreplace(
                                        Producto, 1, element(2, Datos), {Producto, PID}
                                    ),
                                    element(3, Datos),
                                    element(4, Datos)
                                }
                            )
                    end
            end;
        {De, lista_productos} ->
            % Mostrar lista de productos
            io:format("[Tienda]: Listando productos~n"),
            De ! {servidor_tienda, element(2, Datos)},
            servidor(Datos);
        {De, {crea_pedido, Socio, ListaDeProductos}} ->
            % Verificamos que el socio esté suscrito
            case lists:member(Socio, element(1, Datos)) of
                false ->
                    De ! {servidor_tienda, {error, "Socio no suscrito"}},
                    servidor(Datos);
                true ->
                    io:format("[Tienda]: Creando pedido ~p~n", [integer_to_list(element(3, Datos))]),
                    % Creamos una lista de productos con la cantidad deseada tomando en cuenta la disponibilidad
                    NuevaListaProductos = lists:map(
                        fun({Producto, CantidadDeseada}) ->
                            NuevoProducto = lists:keyfind(Producto, 1, element(2, Datos)),
                            case NuevoProducto of
                                false ->
                                    {Producto, 0};
                                {Producto, PID} ->
                                    PID ! {self(), mostrar_disponibilidad},
                                    receive
                                        {servidor_producto, Cantidad} ->
                                            io:format("[Tienda ~p]: Cantidad ~p: ~p~n", [
                                                integer_to_list(element(3, Datos)),
                                                Producto,
                                                Cantidad
                                            ]),
                                            case CantidadDeseada > Cantidad of
                                                true ->
                                                    {Producto, Cantidad};
                                                false ->
                                                    {Producto, CantidadDeseada}
                                            end
                                    end
                            end
                        end,
                        ListaDeProductos
                    ),
                    io:format("[Tienda ~p]: Nueva lista de productos: ~p~n", [
                        integer_to_list(element(3, Datos)), NuevaListaProductos
                    ]),
                    % Actualizamos la cantidad disponible de cada producto
                    lists:foreach(
                        fun({Nombre, Cantidad}) ->
                            Disponibles = llama_producto_desde_servidor(
                                Nombre, mostrar_disponibilidad, element(2, Datos)
                            ),
                            llama_producto_desde_servidor(
                                Nombre,
                                {actualizar_disponibilidad, Disponibles - Cantidad},
                                element(2, Datos)
                            )
                        end,
                        NuevaListaProductos
                    ),
                    % Actualizamos los productos vendidos
                    NuevaListaProductosVendidos = lists:map(
                        fun({Producto, CantidadDeseada}) ->
                            NuevoProducto = lists:keyfind(Producto, 1, element(4, Datos)),
                            case NuevoProducto of
                                false ->
                                    {Producto, CantidadDeseada};
                                {Producto, CantidadActual} ->
                                    {Producto, CantidadActual + CantidadDeseada}
                            end
                        end,
                        NuevaListaProductos
                    ),

                    De ! {servidor_tienda, ok},
                    servidor({
                        element(1, Datos),
                        element(2, Datos),
                        element(3, Datos) + 1,
                        NuevaListaProductosVendidos
                    })
            end;
        {De, productos_vendidos} ->
            % Mostrar lista de productos vendidos
            io:format("[Tienda]: Listando productos vendidos~n"),
            Vendidos = element(4, Datos),
            io:format("[Tienda]: ~p~n", [Vendidos]),
            De ! {servidor_tienda, Vendidos},
            servidor(Datos);
        {'DOWN', _, process, PID, _} ->
            % Si se cae un servidor de producto, se elimina de la lista de productos
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
            % Mensaje no reconocido
            De ! {servidor_tienda, {error, "Mensaje no reconocido"}},
            servidor(Datos)
    end.

abre_tienda() ->
    % Función para abrir la tienda
    % Se lanza el proceso y se registra con el nombre servidor_tienda
    register(
        servidor_tienda,
        spawn(servidor_tienda, servidor, [{[], [], 0, []}])
    ).

cierra_tienda() ->
    % Función para cerrar la tienda
    % Se termina el proceso y se elimina el registro
    exit(whereis(servidor_tienda), kill),
    unregister(servidor_tienda).

lista_socios() ->
    % Función para listar los socios
    llama_tienda(lista_socios).

registra_producto(Producto, Cantidad) ->
    % Función para registrar un producto
    llama_tienda({registra_producto, Producto, Cantidad}).

productos_vendidos() ->
    % Función para listar los productos vendidos
    llama_tienda(productos_vendidos).

elimina_producto(Producto) ->
    % Función para eliminar un producto
    llama_tienda({elimina_producto, Producto}).

modifica_producto(Producto, Cantidad) ->
    % Función para modificar un producto
    llama_tienda({modificar_producto, Producto, Cantidad}).

servidor_producto(Cantidad) ->
    % Función para manejar un producto
    receive
        {De, mostrar_disponibilidad} ->
            % Mostrar disponibilidad
            io:format("[Servidor producto]: Mostrando disponibilidad~n"),
            De ! {servidor_producto, Cantidad},
            servidor_producto(Cantidad);
        {De, {actualizar_disponibilidad, CantidadNueva}} ->
            % Actualizar disponibilidad
            io:format("[Servidor producto](1/2): Actualizando disponibilidad~n"),
            io:format("[Servidor producto](2/2): Cantidad: ~p~n", [CantidadNueva]),
            De ! {servidor_producto, ok},
            servidor_producto(CantidadNueva);
        {De, _} ->
            % Mensaje no reconocido
            De ! {servidor_producto, {error, "Mensaje no reconocido"}},
            servidor_producto(Cantidad)
    end.

crear_servidor_producto(Cantidad) ->
    % Función para crear un servidor de producto
    % Se lanza el proceso con la cantidad inicial
    spawn(servidor_tienda, servidor_producto, [Cantidad]).

llama_producto(Producto, Mensaje) ->
    % Función para llamar a un producto
    case Producto of
        {_, PID} ->
            % Si se recibe un PID, se llama directamente
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
            % Si se recibe un nombre, se busca en la lista de productos
            io:format("Buscando ~p~n", [Producto]),
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

llama_producto_desde_servidor(Producto, Mensaje, ListaProductos) ->
    % Función para llamar a un producto desde el servidor
    % Se necesita esta funcion para evitar un deadlock al buscar la lista de productos desde el servidor
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
            io:format("Buscando ~p~n", [Producto]),
            {_, PID} = lists:keyfind(Producto, 1, ListaProductos),
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
    % Crea pedido
    io:format("~p~n", [llama_tienda({crea_pedido, sebastian, [{peras, 1}, {manzanas, 50}]})]),
    io:format("~n~n~n~n"),
    % Crea otro pedido
    io:format("Creando segundo pedido~n"),
    io:format("~p~n", [llama_tienda({crea_pedido, pepe, [{peras, 1}, {manzanas, 50}]})]),

    io:format("~n~n~n~n"),
    % Productos vendidos
    io:format("~p~n", [productos_vendidos()]),

    % Elimina suscripción de sebastian
    io:format("~p~n", [llama_tienda({eliminar_suscripcion, sebastian})]),
    % Lista los socios
    io:format("~p~n", [lista_socios()]),
    cierra_tienda().
