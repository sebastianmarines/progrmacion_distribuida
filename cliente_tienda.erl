-module(cliente_tienda).
-export([
    suscribir_socio/1, elimina_socio/1, lista_existencias/0, crea_pedido/2
]).
-compile(common).
-import(common, [llama_tienda/1]).

suscribir_socio(Cliente) ->
    llama_tienda({suscribir, Cliente}).

elimina_socio(Cliente) ->
    llama_tienda({eliminar_suscripcion, Cliente}).

lista_existencias() ->
    llama_tienda(lista_productos).

crea_pedido(Socio, ListaDeProductos) ->
    llama_tienda({crea_pedido, Socio, ListaDeProductos}).
