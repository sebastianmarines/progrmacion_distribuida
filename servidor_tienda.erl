-module(servidor_tienda).
-export([inicio/0, servidor/1]).

servidor(Datos) ->
    receive
        {De, {suscribir, Quien}} ->
            io:format("Suscribiendo a ~p~n", [Quien]),
            io:format("Datos: ~p~n", [Datos]),
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
            io:format("Datos: ~p~n", [Datos]),
            io:format("Eliminando suscripción de ~p~n", [Quien]),
            De ! {servidor_tienda, ok},
            servidor(Datos)
    end.

inicio() ->
    register(
        servidor_tienda,
        spawn(servidor_tienda, servidor, [{[], []}])
    ).

% suscribe(Cliente) ->
%     servidor_tienda ! {self(), {suscribir, Cliente}},
%     receive
%         {servidor_tienda, ok} ->
%             io:format("Suscripción realizada~n");
%         {servidor_tienda, {error, Cliente}} ->
%             io:format("El cliente ~p ya está suscrito~n", [Cliente])
%     end.
