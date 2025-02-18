# Revision history for tailwind (Haskell)

## Unreleased (0.4.0.0)

- Nix
  - GHC 9 support
  - Explicitly specify nixpkgs input in flake
- Log path to the tailwind binary used.
- #13: Allowing passing plugins in CLI
- #23: Due to upstream change, we now look for `tailwindcss` binary

## 0.3.0.0

- Nix:
    - Switch to official nixpkgs package for TailwindCSS and its plugins
    - Use relude 1.0
- Switch from lens to `optics-core`

## 0.2.0.0

- Add `--output`
- Fix quoted source paths breaking tailwind.config.js syntax
- Workaround Tailwind not crashing with non-zero exitcode

## 0.1.0.0

* First version.
