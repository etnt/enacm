
use rustler::{NifResult, Binary};
use nacm_validator::{NacmConfig, AccessRequest, Operation, RequestContext};
use serde::{Deserialize, Serialize};
use serde_json;
use std::sync::Mutex;

rustler::init!("nacm_nif");

// Global storage for cached NACM configuration
static CACHED_CONFIG: Mutex<Option<NacmConfig>> = Mutex::new(None);

/// Serializable version of AccessRequest for JSON parsing
#[derive(Debug, Deserialize, Serialize)]
struct SerializableAccessRequest {
    user: String,
    module_name: Option<String>,
    rpc_name: Option<String>,
    operation: String,
    path: Option<String>,
    context: Option<String>,
    command: Option<String>,
}

impl SerializableAccessRequest {
    /// Convert to AccessRequest with borrowed strings and handle context separately
    fn to_access_request_and_context(&self) -> Result<(AccessRequest, Option<RequestContext>), String> {
        let operation = match self.operation.as_str() {
            "read" => Operation::Read,
            "create" => Operation::Create,
            "update" => Operation::Update,
            "delete" => Operation::Delete,
            "exec" => Operation::Exec,
            _ => return Err(format!("Invalid operation: {}", self.operation)),
        };

        // Parse context if provided and return it separately
        let context = if let Some(ctx_str) = &self.context {
            Some(match ctx_str.to_lowercase().as_str() {
                "netconf" => RequestContext::NETCONF,
                "cli" => RequestContext::CLI,
                "webui" => RequestContext::WebUI,
                other => RequestContext::Other(other.to_string()),
            })
        } else {
            None
        };

        // We'll create the AccessRequest in the calling function to handle lifetimes properly
        Ok((AccessRequest {
            user: &self.user,
            module_name: self.module_name.as_deref(),
            rpc_name: self.rpc_name.as_deref(),
            operation,
            path: self.path.as_deref(),
            context: None, // Will be set in the calling function
            command: self.command.as_deref(),
        }, context))
    }
}

/// Validate an access request against NACM rules with caching.
///
/// `config_xml`: NACM config as XML string or binary. If empty, uses cached config.
/// `request_json`: AccessRequest as JSON string or binary
/// Returns: {permitted, should_log} tuple where:
///   - permitted: true if permit, false if deny
///   - should_log: true if this decision should be logged
///
/// Caching behavior:
/// 1. If no config cached and config_xml is non-empty: parse and cache config
/// 2. If config_xml is empty: use cached config (if available)
/// 3. If config_xml is non-empty: parse new config and update cache
#[rustler::nif]
fn validate(config_xml: Binary, request_json: Binary) -> NifResult<(bool, bool)> {
    // Convert binaries to strings
    let config_str = match std::str::from_utf8(&config_xml) {
        Ok(s) => s,
        Err(_) => return Ok((false, false)),
    };
    
    let request_str = match std::str::from_utf8(&request_json) {
        Ok(s) => s,
        Err(_) => return Ok((false, false)),
    };

    // Determine which config to use based on caching logic
    let config = if config_str.trim().is_empty() {
        // Use cached config if available
        match CACHED_CONFIG.lock() {
            Ok(cache) => {
                match cache.as_ref() {
                    Some(cached_config) => cached_config.clone(),
                    None => return Ok((false, false)), // No cached config available
                }
            }
            Err(_) => return Ok((false, false)), // Mutex error
        }
    } else {
        // Parse new config and update cache
        let new_config = match NacmConfig::from_xml(config_str) {
            Ok(cfg) => cfg,
            Err(_) => return Ok((false, false)),
        };

        // Update cache with new config
        match CACHED_CONFIG.lock() {
            Ok(mut cache) => {
                *cache = Some(new_config.clone());
                new_config
            }
            Err(_) => return Ok((false, false)), // Mutex error
        }
    };

    // Parse AccessRequest from JSON
    let serializable_req: SerializableAccessRequest = match serde_json::from_str(request_str) {
        Ok(r) => r,
        Err(_) => return Ok((false, false)),
    };

    // Convert to AccessRequest and get context
    let (mut req, context) = match serializable_req.to_access_request_and_context() {
        Ok(r) => r,
        Err(_) => return Ok((false, false)),
    };

    // Set the context reference
    req.context = context.as_ref();

    // Validate using the selected config
    let result = config.validate(&req);
    Ok((
        matches!(result.effect, nacm_validator::RuleEffect::Permit),
        result.should_log
    ))
}
