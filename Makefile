# Clones nacm-rust-prototype if not present and builds nacm_nif (Erlang NIF)

NACM_RUST_PROTO_REPO = https://github.com/etnt/nacm-rust-prototype.git
NACM_NIF_DIR = nacm_nif

.PHONY: all nif clean test

all: nacm-rust-prototype nif

# Clone the nacm-rust-prototype repo if not already present
nacm-rust-prototype:
	git clone $(NACM_RUST_PROTO_REPO)

# Build the nacm_nif Erlang NIF (using rebar3)
nif: nacm-rust-prototype
	cd $(NACM_NIF_DIR) && rebar3 compile

clean:
	cd $(NACM_NIF_DIR) && rebar3 clean

test:
	(cd nacm_nif/test; ../_build/default/lib/lux/bin/lux run.lux)
