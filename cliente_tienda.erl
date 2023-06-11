-module(cliente_tienda).
-export([suscribe/1, unsuscribe/1]).
-import(common, [llama_tienda/1]).

suscribe(Cliente) ->
    llama_tienda({suscribir, Cliente}).

unsuscribe(Cliente) ->
    llama_tienda({eliminar_suscripcion, Cliente}).
