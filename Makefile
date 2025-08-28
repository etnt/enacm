# Clones nacm-validator if not present and builds nacm_nif (Erlang NIF)

NACM_VALIDATOR_REPO = https://github.com/etnt/nacm-validator.git
NACM_NIF_DIR = nacm_nif

.PHONY: all nif clean test

all: nacm-validator nif

# Clone the nacm-validator repo if not already present
nacm-validator:
	git clone -b tailf-acm $(NACM_VALIDATOR_REPO)

# Build the nacm_nif Erlang NIF (using rebar3)
nif: nacm-validator
	cd $(NACM_NIF_DIR) && rebar3 compile

clean:
	cd $(NACM_NIF_DIR) && rebar3 clean

test:
	(cd nacm_nif/test; ../_build/default/lib/lux/bin/lux run.lux)
