# Clones nacm-validator if not present and builds nacm_nif (Erlang NIF)

NACM_VALIDATOR_REPO = https://github.com/etnt/nacm-validator.git
NACM_NIF_DIR = nacm_nif

# Architecture detection and override
# Set FORCE_NATIVE=1 to always use arm64 on Apple Silicon regardless of detection
ifdef FORCE_NATIVE
    $(info 🍎 FORCE_NATIVE=1: Building for Apple Silicon arm64)
    export BUILD_ARCH := arm64
endif

# Manual architecture override
ifdef BUILD_ARCH
    $(info � BUILD_ARCH=$(BUILD_ARCH): Using manual architecture override)
    export BUILD_ARCH
endif

.PHONY: all nif clean test

all: nacm-validator nif

# Debug target to show architecture detection
.PHONY: arch-info
arch-info:
	@echo "Architecture Detection Info:"
	@echo "  Reported arch (uname -m): $$(uname -m)"
	@echo "  Hardware arch (sysctl):   $$(sysctl -n hw.machine 2>/dev/null || echo 'N/A')"
	@echo "  OS:                       $$(uname -s)"
	@if [ -n "$(BUILD_ARCH)" ]; then \
		echo "  BUILD_ARCH override:      $(BUILD_ARCH)"; \
	else \
		UNAME_S=$$(uname -s); \
		UNAME_M=$$(uname -m); \
		if [ "$$UNAME_S" = "Darwin" ] && [ "$$UNAME_M" = "x86_64" ]; then \
			HW_MACHINE=$$(sysctl -n hw.machine 2>/dev/null || echo ""); \
			if [ "$$HW_MACHINE" = "arm64" ]; then \
				echo "  BUILD_ARCH override:      arm64 (auto-detected Rosetta)"; \
			else \
				echo "  BUILD_ARCH override:      (none - native x86_64)"; \
			fi; \
		else \
			echo "  BUILD_ARCH override:      (none - using system default)"; \
		fi; \
	fi

# Clone the nacm-validator repo if not already present
nacm-validator:
	git clone -b tailf-acm $(NACM_VALIDATOR_REPO)

# Build the nacm_nif Erlang NIF (using rebar3)
nif: nacm-validator
	@UNAME_S=$$(uname -s); \
	UNAME_M=$$(uname -m); \
	BUILD_ARCH_OVERRIDE="$(BUILD_ARCH)"; \
	if [ "$$UNAME_S" = "Darwin" ] && [ "$$UNAME_M" = "x86_64" ] && [ -z "$$BUILD_ARCH_OVERRIDE" ]; then \
		HW_MACHINE=$$(sysctl -n hw.machine 2>/dev/null || echo ""); \
		if [ "$$HW_MACHINE" = "arm64" ]; then \
			echo "🔄 Auto-detected: Rosetta emulation on Apple Silicon, using native arm64"; \
			BUILD_ARCH_OVERRIDE="arm64"; \
		fi; \
	fi; \
	if [ -n "$$BUILD_ARCH_OVERRIDE" ]; then \
		cd $(NACM_NIF_DIR) && BUILD_ARCH=$$BUILD_ARCH_OVERRIDE rebar3 compile; \
	else \
		cd $(NACM_NIF_DIR) && rebar3 compile; \
	fi

clean:
	cd $(NACM_NIF_DIR) && rebar3 clean

.PHONY: test
test: test_nacm test_tailf_acm

.PHONY: test_nacm
test_nacm:
	(cd nacm_nif/test; ../_build/default/lib/lux/bin/lux nacm.lux)

.PHONY: test_tailf_acm
test_tailf_acm:
	(cd nacm_nif/test; ../_build/default/lib/lux/bin/lux tailf-acm.lux)

# Example targets
.PHONY: examples
examples: nif
	(cd nacm_nif; erl -pa _build/default/lib/*/ebin -eval "nacm_nif_example:run_all_examples(), halt()." -noshell)

.PHONY: example_permit
example_permit: nif
	(cd nacm_nif; erl -pa _build/default/lib/*/ebin -eval "nacm_nif_example:permit_example(), halt()." -noshell)

.PHONY: example_deny
example_deny: nif
	(cd nacm_nif; erl -pa _build/default/lib/*/ebin -eval "nacm_nif_example:deny_example(), halt()." -noshell)

.PHONY: example_cache
example_cache: nif
	(cd nacm_nif; erl -pa _build/default/lib/*/ebin -eval "nacm_nif_example:cache_example(), halt()." -noshell)

.PHONY: example_performance
example_performance: nif
	(cd nacm_nif; erl -pa _build/default/lib/*/ebin -eval "nacm_nif_example:performance_comparison(), halt()." -noshell)

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all                 - Build everything (clone nacm-validator + compile NIF)"
	@echo "  nif                 - Build the NIF library (auto-detects architecture)"
	@echo "  clean               - Clean build artifacts"
	@echo ""
	@echo "Architecture (automatically handled):"
	@echo "  - Auto-detects Apple Silicon vs Intel/Rosetta"
	@echo "  - Force native: FORCE_NATIVE=1 make nif"
	@echo "  - Manual override: BUILD_ARCH=arm64 make nif"
	@echo "  - Debug info: make arch-info"
	@echo ""
	@echo "Testing:"
	@echo "  test                - Run all tests (NACM + Tail-f ACM)"
	@echo "  test_nacm           - Run standard NACM tests only"
	@echo "  test_tailf_acm      - Run Tail-f ACM extension tests only"
	@echo ""
	@echo "Examples:"
	@echo "  examples            - Run all examples with assertions"
	@echo "  example_permit      - Run permit example (admin user)"
	@echo "  example_deny        - Run deny example (guest user)"
	@echo "  example_cache       - Run caching functionality example"
	@echo "  example_performance - Run performance comparison example"
	@echo ""
	@echo "  help                - Show this help message"
