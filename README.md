# ENACM - Erlang NACM NIF Library

An Erlang Native Implemented Function (NIF) library for validating requests according to NACM (Network Access Control Model) rules as defined in RFC 8341.

This project integrates the [nacm-rust-prototype](https://github.com/etnt/nacm-rust-prototype) Rust library into Erlang via a NIF, providing high-performance NACM validation for Erlang applications.

## Features

- **NACM Validation**: Full RFC 8341 compliance for access control validation
- **XML Configuration**: Parse real-world NACM XML configurations
- **JSON Requests**: Accept access requests in JSON format
- **High Performance**: Native Rust implementation via NIF
- **Easy Integration**: Simple Erlang API

## Prerequisites

- Erlang/OTP (22 or later recommended)
- Rust toolchain (1.70 or later)
- rebar3
- Git

## Building

The build system automatically detects your architecture and compiles the appropriate NIF library:

- **Apple Silicon (ARM64)**: Compiles for `aarch64-apple-darwin`
- **Intel Mac (x86_64)**: Compiles for `x86_64-apple-darwin`
- **Linux x86_64**: Compiles for `x86_64-unknown-linux-gnu`
- **Linux ARM64**: Compiles for `aarch64-unknown-linux-gnu`

### Quick Start

```bash
# Clone and build everything
make

# Or step by step:
make nacm-rust-prototype  # Clone the Rust NACM library
make nif                  # Build the NIF library (auto-detects architecture)
```

### Manual Build

```bash
# 1. Clone the NACM Rust library dependency
git clone https://github.com/etnt/nacm-rust-prototype.git

# 2. Build the NIF (auto-detects architecture)
cd nacm_nif
rebar3 compile

# Or build directly with the script
./build_nif.sh
```

## Usage

### Basic Example

```erlang
% Start Erlang shell in nacm_nif directory
cd nacm_nif
erl -pa _build/default/lib/*/ebin

% In Erlang shell:
1> nacm_nif_example:permit_example().
true

2> nacm_nif_example:deny_example().
false

% Or use manual JSON examples (no jsx dependency):
3> nacm_nif_example:permit_example_manual().
true

4> nacm_nif_example:deny_example_manual().
false
```

### Direct API Usage

```erlang
% Load NACM configuration (XML string)
ConfigXml = "<?xml version=\"1.0\"?>
<config xmlns=\"urn:ietf:params:xml:ns:yang:ietf-netconf-acm\">
  <nacm>
    <enable-nacm>true</enable-nacm>
    <read-default>deny</read-default>
    <write-default>deny</write-default>
    <exec-default>deny</exec-default>
    <groups>
      <group>
        <name>admin</name>
        <user-name>admin</user-name>
      </group>
    </groups>
    <rule-list>
      <name>admin-rules</name>
      <group>admin</group>
      <rule>
        <name>permit-admin-exec</name>
        <access-operations>exec</access-operations>
        <action>permit</action>
        <rpc-name>edit-config</rpc-name>
      </rule>
    </rule-list>
  </nacm>
</config>",

% Create access request (JSON string)
RequestJson = jsx:encode(#{
    user => "admin",
    module_name => null,
    rpc_name => "edit-config", 
    operation => "exec",
    path => null
}),

% Validate the request
Result = nacm_nif:validate(ConfigXml, RequestJson).
% Result will be 'true' (permit) or 'false' (deny)
```

## JSON Request Format

Access requests should be JSON objects with the following fields:

```json
{
  "user": "username",
  "module_name": "yang-module-name",
  "rpc_name": "rpc-operation-name", 
  "operation": "read|create|update|delete|exec",
  "path": "/xpath/to/resource"
}
```

**Fields:**
- `user` (required): String - Username making the request
- `module_name` (optional): String or null - YANG module name
- `rpc_name` (optional): String or null - RPC operation name
- `operation` (required): String - One of: "read", "create", "update", "delete", "exec"
- `path` (optional): String or null - XPath to the resource

## Examples

The project includes several examples:

### 1. Permit Example
```erlang
nacm_nif_example:permit_example().
```
Tests an admin user performing an allowed exec operation.

### 2. Deny Example  
```erlang
nacm_nif_example:deny_example().
```
Tests a guest user performing a denied exec operation.

### 3. Custom Validation
```erlang
% Create your own config and request
nacm_nif:validate(YourConfigXml, YourRequestJson).
```

## Testing

The project includes a comprehensive Lux test suite that validates all functionality:

### Running Tests

```bash
# Run all tests
make test
```

The test suite includes 11 comprehensive tests covering:

- **Basic functionality**: permit/deny examples with jsx and manual JSON encoding
- **Direct API calls**: Testing `nacm_nif:validate/2` directly
- **Error handling**: Invalid XML configs and JSON requests
- **Edge cases**: Unknown users, different RPC operations
- **jsx encoding verification**: Binary vs string encoding correctness

### Test Details

1. **Test 1-4**: Basic examples (jsx/manual JSON, permit/deny)
2. **Test 5-6**: Direct API usage (valid admin, invalid guest)
3. **Test 7-8**: Error handling (bad XML, bad JSON)
4. **Test 9**: jsx binary encoding verification
5. **Test 10**: Unknown user "bill" denial
6. **Test 11**: Different RPC operation denial

All tests run automatically and provide detailed progress output. Test logs are stored in `nacm_nif/test/lux_logs/` with HTML reports for detailed analysis.

### Test Requirements

The Lux test framework is automatically installed as a dependency via rebar3. No additional setup is required beyond the standard build prerequisites.

## Project Structure

```
enacm/
├── Makefile                    # Top-level build automation
├── README.md                   # This file
├── nacm-rust-prototype/        # Rust NACM library (cloned)
└── nacm_nif/                   # Erlang NIF application
    ├── src/
    │   ├── nacm_nif.erl        # Main NIF module
    │   ├── nacm_nif_example.erl # Usage examples
    │   ├── nacm_nif.app.src    # Application resource file
    │   ├── nacm_nif_app.erl    # Application behavior
    │   └── nacm_nif_sup.erl    # Supervisor
    ├── test/                   # Lux test suite
    │   ├── run.lux             # Main test file (11 comprehensive tests)
    │   └── common.luxinc       # Common test utilities
    ├── native/nacm_nif/        # Rust NIF implementation
    │   ├── src/lib.rs          # NIF functions
    │   └── Cargo.toml          # Rust dependencies
    ├── priv/                   # Compiled NIF library
    └── rebar.config            # Erlang build config
```

## API Reference

### nacm_nif:validate/2

```erlang
validate(ConfigXml, RequestJson) -> boolean().
```

Validates an access request against NACM rules.

**Parameters:**
- `ConfigXml` (string): NACM configuration as XML string
- `RequestJson` (string): Access request as JSON string

**Returns:**
- `true` if access is permitted
- `false` if access is denied

## Development

### Cross-Compilation

The build system automatically detects and compiles for your architecture. If you need to cross-compile for a different architecture, you can modify the `build_nif.sh` script or build manually:

```bash
# Example: Compile for Intel Mac from Apple Silicon
cd nacm_nif/native/nacm_nif
rustup target add x86_64-apple-darwin
cargo build --release --target x86_64-apple-darwin
cp target/x86_64-apple-darwin/release/libnacm_nif.dylib ../../priv/nacm_nif.so
```

### Architecture Detection

The build script (`nacm_nif/build_nif.sh`) automatically:
1. Detects your OS and architecture using `uname`
2. Maps to the appropriate Rust target triple
3. Installs the target if needed (`rustup target add`)
4. Compiles the NIF library
5. Copies it to the correct location with proper naming

### Troubleshooting

**Architecture Mismatch Error:**
If you see an error like "incompatible architecture (have 'x86_64', need 'arm64')", clean and rebuild:

```bash
cd nacm_nif
rm -rf priv/nacm_nif.so native/nacm_nif/target
rebar3 compile
```

### Rebuilding

```bash
# Clean and rebuild
make clean
make

# Or just the NIF
cd nacm_nif
rebar3 clean compile
```

### Running Tests

See the [Testing](#testing) section above for comprehensive test coverage using the Lux test framework.

## Dependencies

- **[nacm-rust-prototype](https://github.com/etnt/nacm-rust-prototype)**: Rust NACM validation library
- **rustler**: Rust NIF framework
- **serde/serde_json**: JSON serialization
- **jsx** (optional): For JSON encoding in Erlang examples

## License

This project follows the same license as the nacm-rust-prototype dependency (Mozilla Public License 2.0).

## References

- [RFC 8341 - Network Configuration Access Control Model](https://tools.ietf.org/rfc/rfc8341.txt)
- [nacm-rust-prototype](https://github.com/etnt/nacm-rust-prototype)
- [Rustler Documentation](https://docs.rs/rustler/)
