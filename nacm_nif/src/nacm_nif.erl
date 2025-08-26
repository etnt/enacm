-module(nacm_nif).
-on_load(init/0).

%% NIF function stubs
-export([validate/2]).

init() ->
    PrivDir = code:priv_dir(nacm_nif),
    NifPath = filename:join([PrivDir, "nacm_nif"]),
    case erlang:load_nif(NifPath, 0) of
        ok ->
            ok;
        {error, Reason} ->
            error({nif_load_failed, Reason})
    end.

validate(_Request, _Rules) ->
    erlang:nif_error(nif_not_loaded).
