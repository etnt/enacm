-module(nacm_nif).
-on_load(init/0).

%% NIF function stubs
-export([validate/2, validate_with_cache/1, set_config/1]).

%% Type exports
-export_type([nacm_result/0, config_xml/0, request_json/0]).

%% Type specifications
-type nacm_result() :: {Permitted :: boolean(), ShouldLog :: boolean()}.
-type config_xml() :: binary().
-type request_json() :: binary().

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
-spec validate(config_xml(), request_json()) -> nacm_result() | false.
validate(_Config, _Request) ->
    erlang:nif_error(nif_not_loaded).

%% Convenience function: validate using cached config (empty config string)
-spec validate_with_cache(request_json()) -> nacm_result() | false.
validate_with_cache(Request) ->
    validate(<<>>, Request).

%% Convenience function: set/update the cached config
-spec set_config(config_xml()) -> ok.
set_config(Config) ->
    DummyRequest = <<"{\"user\":\"dummy\",\"operation\":\"read\"}">>,
    % This will parse and cache the config, the validation result doesn't matter
    validate(Config, DummyRequest),
    ok.
