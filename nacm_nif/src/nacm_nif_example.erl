%% Example usage of nacm_nif:validate/2 and caching functionality
%%
%% 1. Load NACM XML config (as string)
%% 2. Build JSON request (as string)
%% 3. Call nacm_nif:validate/2 or use caching features

-module(nacm_nif_example).
-export([
    permit_example/0,
    deny_example/0,
    permit_example_manual/0,
    deny_example_manual/0,
    cache_example/0,
    performance_comparison/0
]).

%% Examples using jsx for JSON encoding
permit_example() ->
    ConfigXml = sample_config(),
    %% Example: user 'admin' with exec operation on 'edit-config' RPC
    %% Use binaries to avoid jsx treating strings as character arrays
    Request = #{
        user => <<"admin">>,
        module_name => null,
        rpc_name => <<"edit-config">>,
        operation => <<"exec">>,
        path => null
    },
    RequestJson = jsx:encode(Request),
    nacm_nif:validate(list_to_binary(ConfigXml), RequestJson).

deny_example() ->
    ConfigXml = sample_config(),
    %% Example: user 'guest' with exec operation on 'edit-config' RPC
    %% Use binaries to avoid jsx treating strings as character arrays
    Request = #{
        user => <<"guest">>,
        module_name => null,
        rpc_name => <<"edit-config">>,
        operation => <<"exec">>,
        path => null
    },
    RequestJson = jsx:encode(Request),
    nacm_nif:validate(list_to_binary(ConfigXml), RequestJson).

%% Examples using manual JSON encoding (no jsx dependency needed)
permit_example_manual() ->
    ConfigXml = sample_config(),
    %% Manual JSON string for admin user
    RequestJson =
        "{\"user\":\"admin\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}",
    nacm_nif:validate(list_to_binary(ConfigXml), list_to_binary(RequestJson)).

deny_example_manual() ->
    ConfigXml = sample_config(),
    %% Manual JSON string for guest user
    RequestJson =
        "{\"user\":\"guest\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}",
    nacm_nif:validate(list_to_binary(ConfigXml), list_to_binary(RequestJson)).

sample_config() ->
    %% Minimal NACM config XML string (permit admin, deny guest)
    "<config xmlns=\"urn:ietf:params:xml:ns:yang:ietf-netconf-acm\">\n"
    "  <nacm>\n"
    "    <enable-nacm>true</enable-nacm>\n"
    "    <read-default>deny</read-default>\n"
    "    <write-default>deny</write-default>\n"
    "    <exec-default>deny</exec-default>\n"
    "    <groups>\n"
    "      <group>\n"
    "        <name>admin</name>\n"
    "        <user-name>admin</user-name>\n"
    "      </group>\n"
    "    </groups>\n"
    "    <rule-list>\n"
    "      <name>admin-rules</name>\n"
    "      <group>admin</group>\n"
    "      <rule>\n"
    "        <name>permit-admin-exec</name>\n"
    "        <access-operations>exec</access-operations>\n"
    "        <action>permit</action>\n"
    "        <rpc-name>edit-config</rpc-name>\n"
    "      </rule>\n"
    "    </rule-list>\n"
    "  </nacm>\n"
    "</config>".

%% Cache example - demonstrates efficient config reuse
cache_example() ->
    ConfigXml = sample_config(),

    % Step 1: Set config in cache
    io:format("Setting config in cache...~n"),
    nacm_nif:set_config(list_to_binary(ConfigXml)),

    % Step 2: Use cached config for multiple validations
    AdminRequest =
        <<"{\"user\":\"admin\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}">>,
    GuestRequest =
        <<"{\"user\":\"guest\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}">>,
    BillRequest =
        <<"{\"user\":\"bill\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}">>,

    % All these calls use cached config (no XML parsing overhead)
    AdminResult = nacm_nif:validate_with_cache(AdminRequest),
    GuestResult = nacm_nif:validate_with_cache(GuestRequest),
    BillResult = nacm_nif:validate_with_cache(BillRequest),

    io:format("Cache validation results:~n"),
    io:format("  Admin: ~p~n", [AdminResult]),
    io:format("  Guest: ~p~n", [GuestResult]),
    io:format("  Bill:  ~p~n", [BillResult]),

    % Return summary
    {AdminResult, GuestResult, BillResult}.

%% Performance comparison between cached and non-cached validation
performance_comparison() ->
    ConfigXml = list_to_binary(sample_config()),
    Request =
        <<"{\"user\":\"admin\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}">>,

    % Set up cache
    nacm_nif:set_config(ConfigXml),

    % Number of iterations for timing
    N = 1000,

    % Time non-cached validation (parses XML every time)
    Start1 = erlang:system_time(microsecond),
    [nacm_nif:validate(ConfigXml, Request) || _ <- lists:seq(1, N)],
    End1 = erlang:system_time(microsecond),
    NonCachedTime = End1 - Start1,

    % Time cached validation (no XML parsing)
    Start2 = erlang:system_time(microsecond),
    [nacm_nif:validate_with_cache(Request) || _ <- lists:seq(1, N)],
    End2 = erlang:system_time(microsecond),
    CachedTime = End2 - Start2,

    % Calculate performance improvement
    SpeedupRatio = NonCachedTime / CachedTime,

    io:format("Performance comparison (~p iterations):~n", [N]),
    io:format("  Non-cached: ~p microseconds~n", [NonCachedTime]),
    io:format("  Cached:     ~p microseconds~n", [CachedTime]),
    io:format("  Speedup:    ~.2fx faster~n", [SpeedupRatio]),

    {NonCachedTime, CachedTime, SpeedupRatio}.
