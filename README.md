# tailwind

Test

Run TailwindCSS CLI [without needing](https://srid.ca/nojs) to touch anything JavaScript. No `input.css` or `tailwind.config.js` is necessary.

```sh-session
nix run github:srid/tailwind-haskell -- 'src/**/*.hs' -o output.css
```

Compiles CSS classes in the input file paths or patterns, and writes to the output CSS file. Pass `-w` to run in JIT watcher mode.

## How to use as Haskell dependency via Nix

[`pkgs.haskellPackages.tailwind`](https://nixpkgs.haskell.page/p/tailwind) already wraps the necessary runtime dependencies (tailwind with plugins). You may use it along with [the static `which` library](https://github.com/obsidiansystems/which).

## Use cases

This package is used in [Emanote](https://github.com/srid/emanote) to compile the CSS file on the generated website, as well as in other [Ema apps](https://github.com/EmaApps).
