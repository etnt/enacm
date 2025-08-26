#!/bin/bash

# Auto-detect architecture and build Rust NIF accordingly

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CARGO_DIR="$SCRIPT_DIR/native/nacm_nif"
PRIV_DIR="$SCRIPT_DIR/priv"

# Detect architecture and OS
ARCH=$(uname -m)
OS=$(uname -s)

echo "Detected: $OS $ARCH"

# Determine the Rust target triple
case "$OS" in
    "Darwin")
        case "$ARCH" in
            "arm64")
                TARGET="aarch64-apple-darwin"
                LIB_NAME="libnacm_nif.dylib"
                ;;
            "x86_64")
                TARGET="x86_64-apple-darwin"
                LIB_NAME="libnacm_nif.dylib"
                ;;
            *)
                echo "Unsupported macOS architecture: $ARCH"
                exit 1
                ;;
        esac
        ;;
    "Linux")
        case "$ARCH" in
            "x86_64")
                TARGET="x86_64-unknown-linux-gnu"
                LIB_NAME="libnacm_nif.so"
                ;;
            "aarch64"|"arm64")
                TARGET="aarch64-unknown-linux-gnu"
                LIB_NAME="libnacm_nif.so"
                ;;
            *)
                echo "Unsupported Linux architecture: $ARCH"
                exit 1
                ;;
        esac
        ;;
    *)
        echo "Unsupported OS: $OS"
        exit 1
        ;;
esac

echo "Building for target: $TARGET"

# Install the target if not already available
cd "$CARGO_DIR"
rustup target add "$TARGET" || true

# Build the Rust library
cargo build --release --target "$TARGET"

# Create priv directory if it doesn't exist
mkdir -p "$PRIV_DIR"

# Copy the library to the expected location
SRC_PATH="$CARGO_DIR/target/$TARGET/release/$LIB_NAME"
DEST_PATH="$PRIV_DIR/nacm_nif.so"

if [ -f "$SRC_PATH" ]; then
    cp "$SRC_PATH" "$DEST_PATH"
    echo "Successfully copied $SRC_PATH to $DEST_PATH"
    
    # Verify the architecture (on macOS)
    if command -v file >/dev/null 2>&1; then
        echo "Library info: $(file "$DEST_PATH")"
    fi
else
    echo "Error: Built library not found at $SRC_PATH"
    exit 1
fi
