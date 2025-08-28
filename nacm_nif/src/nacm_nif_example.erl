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
    performance_comparison/0,
    run_all_examples/0
]).

%% Examples using jsx for JSON encoding
permit_example() ->
    io:format("Running permit_example...~n"),
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
    Result = nacm_nif:validate(list_to_binary(ConfigXml), RequestJson),

    %% Assert expected result: admin should be permitted, no logging by default
    Expected = {true, false},
    case Result of
        Expected ->
            io:format("✓ permit_example PASSED: ~p~n", [Result]),
            Result;
        _ ->
            io:format("✗ permit_example FAILED: expected ~p, got ~p~n", [
                Expected, Result
            ]),
            error({assertion_failed, expected, Expected, got, Result})
    end.

deny_example() ->
    io:format("Running deny_example...~n"),
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
    Result = nacm_nif:validate(list_to_binary(ConfigXml), RequestJson),

    %% Assert expected result: guest should be denied, no logging by default
    Expected = {false, false},
    case Result of
        Expected ->
            io:format("✓ deny_example PASSED: ~p~n", [Result]),
            Result;
        _ ->
            io:format("✗ deny_example FAILED: expected ~p, got ~p~n", [
                Expected, Result
            ]),
            error({assertion_failed, expected, Expected, got, Result})
    end.

%% Examples using manual JSON encoding (no jsx dependency needed)
permit_example_manual() ->
    io:format("Running permit_example_manual...~n"),
    ConfigXml = sample_config(),
    %% Manual JSON string for admin user
    RequestJson =
        "{\"user\":\"admin\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}",
    Result = nacm_nif:validate(
        list_to_binary(ConfigXml), list_to_binary(RequestJson)
    ),

    %% Assert expected result: admin should be permitted, no logging by default
    Expected = {true, false},
    case Result of
        Expected ->
            io:format("✓ permit_example_manual PASSED: ~p~n", [Result]),
            Result;
        _ ->
            io:format(
                "✗ permit_example_manual FAILED: expected ~p, got ~p~n", [
                    Expected, Result
                ]
            ),
            error({assertion_failed, expected, Expected, got, Result})
    end.

deny_example_manual() ->
    io:format("Running deny_example_manual...~n"),
    ConfigXml = sample_config(),
    %% Manual JSON string for guest user
    RequestJson =
        "{\"user\":\"guest\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}",
    Result = nacm_nif:validate(
        list_to_binary(ConfigXml), list_to_binary(RequestJson)
    ),

    %% Assert expected result: guest should be denied, no logging by default
    Expected = {false, false},
    case Result of
        Expected ->
            io:format("✓ deny_example_manual PASSED: ~p~n", [Result]),
            Result;
        _ ->
            io:format(
                "✗ deny_example_manual FAILED: expected ~p, got ~p~n", [
                    Expected, Result
                ]
            ),
            error({assertion_failed, expected, Expected, got, Result})
    end.

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
    io:format("Running cache_example...~n"),
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

    %% Assert expected results

    % Admin should be permitted
    ExpectedAdmin = {true, false},
    % Guest should be denied
    ExpectedGuest = {false, false},
    % Bill (unknown user) should be denied
    ExpectedBill = {false, false},

    Results = [
        {admin, AdminResult, ExpectedAdmin},
        {guest, GuestResult, ExpectedGuest},
        {bill, BillResult, ExpectedBill}
    ],

    lists:foreach(
        fun({User, Actual, Expected}) ->
            case Actual of
                Expected ->
                    io:format("✓ cache_example ~p PASSED: ~p~n", [
                        User, Actual
                    ]);
                _ ->
                    io:format(
                        "✗ cache_example ~p FAILED: expected ~p, got ~p~n", [
                            User, Expected, Actual
                        ]
                    ),
                    error(
                        {assertion_failed, User, expected, Expected, got,
                            Actual}
                    )
            end
        end,
        Results
    ),

    % Return summary
    {AdminResult, GuestResult, BillResult}.

%% Performance comparison between cached and non-cached validation
performance_comparison() ->
    io:format("Running performance_comparison...~n"),
    ConfigXml = list_to_binary(sample_config()),
    Request =
        <<"{\"user\":\"admin\",\"module_name\":null,\"rpc_name\":\"edit-config\",\"operation\":\"exec\",\"path\":null}">>,

    % Set up cache
    nacm_nif:set_config(ConfigXml),

    % Number of iterations for timing
    N = 1000,

    % Time non-cached validation (parses XML every time)
    Start1 = erlang:system_time(microsecond),
    NonCachedResults = [
        nacm_nif:validate(ConfigXml, Request)
     || _ <- lists:seq(1, N)
    ],
    End1 = erlang:system_time(microsecond),
    NonCachedTime = End1 - Start1,

    % Time cached validation (no XML parsing)
    Start2 = erlang:system_time(microsecond),
    CachedResults = [
        nacm_nif:validate_with_cache(Request)
     || _ <- lists:seq(1, N)
    ],
    End2 = erlang:system_time(microsecond),
    CachedTime = End2 - Start2,

    % Calculate performance improvement
    SpeedupRatio = NonCachedTime / CachedTime,

    io:format("Performance comparison (~p iterations):~n", [N]),
    io:format("  Non-cached: ~p microseconds~n", [NonCachedTime]),
    io:format("  Cached:     ~p microseconds~n", [CachedTime]),
    io:format("  Speedup:    ~.2fx faster~n", [SpeedupRatio]),

    %% Assert that both methods return the same results

    % Admin should be permitted
    ExpectedResult = {true, false},
    AllNonCachedSame = lists:all(
        fun(R) -> R =:= ExpectedResult end, NonCachedResults
    ),
    AllCachedSame = lists:all(
        fun(R) -> R =:= ExpectedResult end, CachedResults
    ),

    case {AllNonCachedSame, AllCachedSame} of
        {true, true} ->
            io:format(
                "✓ performance_comparison PASSED: all results consistent~n"
            );
        _ ->
            io:format(
                "✗ performance_comparison FAILED: inconsistent results~n"
            ),
            error({assertion_failed, inconsistent_results})
    end,

    %% Assert that cached version is actually faster
    case SpeedupRatio > 1.0 of
        true ->
            io:format(
                "✓ performance_comparison PASSED: cached is ~.2fx faster~n", [
                    SpeedupRatio
                ]
            );
        false ->
            io:format(
                "⚠ performance_comparison WARNING: cached not faster (ratio: ~.2f)~n",
                [SpeedupRatio]
            )
    end,

    {NonCachedTime, CachedTime, SpeedupRatio}.

%% Run all examples with proper error handling
run_all_examples() ->
    io:format("~n=== Running All NACM NIF Examples ===~n"),

    Examples = [
        {permit_example, fun permit_example/0},
        {deny_example, fun deny_example/0},
        {permit_example_manual, fun permit_example_manual/0},
        {deny_example_manual, fun deny_example_manual/0},
        {cache_example, fun cache_example/0},
        {performance_comparison, fun performance_comparison/0}
    ],

    Results = lists:map(
        fun({Name, Fun}) ->
            io:format("~n--- Running ~p ---~n", [Name]),
            try
                Result = Fun(),
                io:format("✓ ~p completed successfully~n", [Name]),
                {Name, ok, Result}
            catch
                error:Reason:Stacktrace ->
                    io:format("✗ ~p FAILED: ~p~n", [Name, Reason]),
                    io:format("Stacktrace: ~p~n", [Stacktrace]),
                    {Name, error, Reason}
            end
        end,
        Examples
    ),

    %% Summary
    Passed = length([Name || {Name, ok, _} <- Results]),
    Total = length(Results),
    Failed = Total - Passed,

    io:format("~n=== Example Summary ===~n"),
    io:format("Total examples: ~p~n", [Total]),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),

    case Failed of
        0 ->
            io:format("🎉 All examples passed!~n"),
            ok;
        _ ->
            io:format("❌ ~p examples failed~n", [Failed]),
            error({examples_failed, Failed})
    end.
