-module(cliente_tienda).
-export([suscribe/1, unsuscribe/1, lista_existencias/0]).
-compile(common).
-import(common, [llama_tienda/1]).

suscribe(Cliente) ->
    llama_tienda({suscribir, Cliente}).

unsuscribe(Cliente) ->
    llama_tienda({eliminar_suscripcion, Cliente}).

lista_existencias() ->
    llama_tienda(lista_productos).
