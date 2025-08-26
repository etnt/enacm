%% Example usage of nacm_nif:validate/2
%%
%% 1. Load NACM XML config (as string)
%% 2. Build JSON request (as string)
%% 3. Call nacm_nif:validate/2

-module(nacm_nif_example).
-export([
    permit_example/0,
    deny_example/0,
    permit_example_manual/0,
    deny_example_manual/0
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
