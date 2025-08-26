
use rustler::{NifResult, Binary};
use nacm_rust_prototype::{NacmConfig, AccessRequest, RuleEffect, Operation};
use serde::{Deserialize, Serialize};
use serde_json;

rustler::init!("nacm_nif");

/// Serializable version of AccessRequest for JSON parsing
#[derive(Debug, Deserialize, Serialize)]
struct SerializableAccessRequest {
    user: String,
    module_name: Option<String>,
    rpc_name: Option<String>,
    operation: String,
    path: Option<String>,
}

impl SerializableAccessRequest {
    /// Convert to AccessRequest with borrowed strings
    fn to_access_request(&self) -> Result<AccessRequest, String> {
        let operation = match self.operation.as_str() {
            "read" => Operation::Read,
            "create" => Operation::Create,
            "update" => Operation::Update,
            "delete" => Operation::Delete,
            "exec" => Operation::Exec,
            _ => return Err(format!("Invalid operation: {}", self.operation)),
        };

        Ok(AccessRequest {
            user: &self.user,
            module_name: self.module_name.as_deref(),
            rpc_name: self.rpc_name.as_deref(),
            operation,
            path: self.path.as_deref(),
        })
    }
}

/// Validate an access request against NACM rules.
///
/// `config_xml`: NACM config as XML string or binary
/// `request_json`: AccessRequest as JSON string or binary
/// Returns: true if permit, false if deny
#[rustler::nif]
fn validate(config_xml: Binary, request_json: Binary) -> NifResult<bool> {
    // Convert binaries to strings
    let config_str = match std::str::from_utf8(&config_xml) {
        Ok(s) => s,
        Err(_) => return Ok(false),
    };
    
    let request_str = match std::str::from_utf8(&request_json) {
        Ok(s) => s,
        Err(_) => return Ok(false),
    };

    // Parse NACM config from XML
    let config = match NacmConfig::from_xml(config_str) {
        Ok(cfg) => cfg,
        Err(_) => return Ok(false),
    };

    // Parse AccessRequest from JSON
    let serializable_req: SerializableAccessRequest = match serde_json::from_str(request_str) {
        Ok(r) => r,
        Err(_) => return Ok(false),
    };

    // Convert to AccessRequest
    let req = match serializable_req.to_access_request() {
        Ok(r) => r,
        Err(_) => return Ok(false),
    };

    // Validate
    let result = config.validate(&req);
    Ok(matches!(result, RuleEffect::Permit))
}
