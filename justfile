#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell"]
set shell := ["bash", "-cu"]

_default:
    @just --list -u

fix:
    cargo clippy \
        --workspace \
        --all-targets \
        --all-features \
        --fix \
        --allow-dirty \
        --allow-staged \
        --no-deps
    cargo fmt --all
    cargo check --workspace --all-features --all-targets
    cargo clippy \
        --workspace \
        --all-targets \
        --all-features \
        -- --deny warnings
    git status

check-playground args='':
    cargo run -p boron playground/src/index.bo {{ args }} --name std -o playground/build --type library --check-only

build-playground args='':
    cargo run -p boron playground/src/index.bo {{ args }} --name playground -o playground/build --type binary --mode release
