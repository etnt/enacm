-module(nacm_nif).
-on_load(init/0).

%% NIF function stubs
-export([validate/2, validate_with_cache/1, set_config/1]).

init() ->
    PrivDir = code:priv_dir(nacm_nif),
    NifPath = filename:join([PrivDir, "nacm_nif"]),
    case erlang:load_nif(NifPath, 0) of
        ok ->
            ok;
        {error, Reason} ->
            error({nif_load_failed, Reason})
    end.

%% Original validate/2 function - always parses config
validate(_Config, _Request) ->
    erlang:nif_error(nif_not_loaded).

%% Convenience function: validate using cached config (empty config string)
validate_with_cache(Request) ->
    validate(<<>>, Request).

%% Convenience function: set/update the cached config
set_config(Config) ->
    DummyRequest = <<"{\"user\":\"dummy\",\"operation\":\"read\"}">>,
    % This will parse and cache the config, the validation result doesn't matter
    validate(Config, DummyRequest),
    ok.
