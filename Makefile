include rust.mk

RUSTC ?= rustc
RUSTFLAGS ?=

.PHONY : all
all : peg

.PHONY : check
check : check-peg

$(eval $(call RUST_CRATE, ./src/peg/))

