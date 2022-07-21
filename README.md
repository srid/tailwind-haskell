# tailwind

Run TailwindCSS CLI [without needing](https://srid.ca/nojs) to touch anything JavaScript. No `input.css` or `tailwind.config.js` is necessary.

```sh-session
nix run github:srid/tailwind-haskell -- 'src/**/*.hs' -o output.css
```

Compiles CSS classes in the input file paths or patterns, and writes to the output CSS file. Pass `-w` to run in JIT watcher mode.

Used in [Emanote](https://github.com/EmaApps/emanote) and other [Ema apps](https://github.com/EmaApps).
