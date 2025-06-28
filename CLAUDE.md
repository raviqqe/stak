# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Building

```bash
# Build all packages
cargo build

# Build with release profile for testing
cargo build --profile release_test

# Build specific component (e.g., minimal interpreter)
cd cmd/minimal && cargo build --release
```

### Testing

```bash
# Run integration tests (requires Ruby, bundler, and external Scheme interpreters)
./tools/integration_test.sh

# Run specific feature test
./tools/cucumber.sh features/smoke.feature

# Run tests with specific tags
./tools/cucumber.sh -t @smoke features/

# Run unit tests
cargo test
```

### Linting

```bash
# Run Clippy linting
./tools/lint.sh

# Or manually:
cargo clippy
cargo clippy --all-features
```

### Documentation

```bash
# Build documentation website (Astro-based)
cd doc && npm run build

# Run doc development server
cd doc && npm run dev
```

### WASM

```bash
# Build WASM package
cd wasm && cargo build --target wasm32-unknown-unknown
```

## Architecture

### Core Components

**Virtual Machine (`vm/`)**

- Core VM implementation in `vm/src/vm.rs`
- Memory management with garbage collection in `vm/src/memory.rs`
- Stack-based execution model with bytecode instructions
- Values are represented as tagged unions in `vm/src/value.rs`

**Engine (`engine/`)**

- High-level scripting API that wraps the VM
- Primary entry point for embedding Stak Scheme in Rust applications
- Handles initialization and execution of Scheme modules

**Compiler (`compiler/`)**

- Compiles R7RS Scheme source to bytecode
- Self-hosted compiler (compiles itself using included bytecode)
- Entry points: `compile_r7rs()` for full R7RS, `compile_bare()` for minimal Scheme

**Module System (`module/`)**

- Static modules for compile-time embedding
- Universal modules for runtime loading
- Hot-reload support for development

### Build System

**Multi-crate Workspace**

- Root `Cargo.toml` defines workspace with 30+ member crates
- Each major component (vm, engine, compiler, etc.) is a separate crate
- Shared configuration and linting rules across workspace

**Build Scripts**

- Scheme source files are compiled to bytecode at build time
- `build.rs` scripts use the compiler to generate embedded bytecode
- Macro system for compile-time Scheme code inclusion

### Device and Platform Abstraction

**Device Layer (`device/`)**

- Abstracts I/O operations (stdio, buffers, void)
- Platform-specific implementations for libc vs void (no-std)

**File System (`file/`)**

- Pluggable file system abstraction
- Implementations: OS-based, memory-based, void (no-op)

**Process Context (`process_context/`)**

- Environment variables, command line args, exit handling
- Platform-specific implementations

### R7RS Implementation

**Standard Library (`r7rs/`)**

- Implements R7RS-small standard
- Modular primitive sets for different language features
- Built on top of VM primitives

**Feature Modules**

- `time/`: Time and clock operations
- `inexact/`: Floating-point number support
- `native/`: Built-in list and equality operations

### Testing

**Cucumber-based Integration Tests**

- Feature files in `features/` directory describe test scenarios
- Each feature tests specific language constructs or libraries
- Tests run against multiple Scheme interpreters for compatibility
- Temporary test execution in `tmp/` directories

**Benchmark Suite (`bench/`)**

- Performance comparison with other languages (Python, Lua)
- Computational benchmarks (Fibonacci, integer sum, Tak function)
- Startup time benchmarks

## Development Notes

### Scheme Code Conventions

- R7RS-small compatibility is maintained
- Self-hosted compiler written in Scheme (see `compiler/src/compile.scm`)
- Standard prelude provides R7RS libraries (see `compiler/src/prelude.scm`)

### Rust Code Conventions

- `#![no_std]` support throughout most crates
- Extensive use of `#![deny(warnings)]` and strict Clippy lints
- Memory-safe VM implementation with custom garbage collector
- Tagged union pattern for Scheme values with efficient representations

### Testing Approach

- Integration tests via Cucumber ensure R7RS compliance
- Cross-interpreter compatibility testing
- Performance regression testing via GitHub Actions
- Snapshot testing for VM memory states
