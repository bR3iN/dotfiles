RUST_TOOLCHAIN ?= stable

.PHONY: rust
rust: $(STAMP)/rustup

$(STAMP)/rustup:
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
		| sh -s -- -y --default-host x86_64-unknown-linux-gnu --default-toolchain "$(RUST_TOOLCHAIN)" --profile default --no-modify-path
