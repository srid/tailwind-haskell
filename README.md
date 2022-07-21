# tailwind

Run TailwindCSS CLI [without needing](https://srid.ca/nojs) to touch anything JavaScript. No `input.css` or `tailwind.config.js` is necessary.

```sh-session
nix run github:srid/tailwind-haskell -- 'src/**/*.hs' -o output.css
```

Compiles CSS classes in the input file paths or patterns, and writes to the output CSS file. Pass `-w` to run in JIT watcher mode.

## How to use as Haskell dependency via Nix

https://github.com/EmaApps/emanima/blob/9482d614d4af48089c256dfccd85c479e345696d/flake.nix

## Use cases

This package is used in [Emanote](https://github.com/EmaApps/emanote) to compile the CSS file on the generated website, as well as in other [Ema apps](https://github.com/EmaApps).
