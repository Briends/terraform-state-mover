# Terraform State Mover

<!-- Badge "not ready to use" -->
![not ready to use](https://img.shields.io/badge/ready%20to%20use-no-red.svg)

This is a simple tool to detect simply moved terraform resources and update the state file accordingly.
If you refactor & modularize your terraform regularly, you might have noticed that terraform does not handle moved resources very well. It improved since
[terraform v1.1](https://developer.hashicorp.com/terraform/language/modules/develop/refactoring).

## Usage

> **NOTE**: The tool relies on a existing terraform binary.

Via nix:

```bash
nix run github:briends/terraform-state-mover
```

## Development

Show flake outputs:

```bash
nix flake show  --allow-import-from-derivation
```

Build:

```bash
nix build
```

Develop with cabal, hlint, haskell-language-server:

```bash
nix develop .
```
