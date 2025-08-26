%%% Application behaviour stub
-module(nacm_nif_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    nacm_nif_sup:start_link().

stop(_State) ->
    ok.
