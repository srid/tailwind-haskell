# tailwind

Run TailwindCSS CLI [without needing](https://www.srid.ca/nojs) to touch anything JavaScript. No `input.css` or `tailwind.config.js` necessary.

```
nix run github:srid/tailwind-haskell -- 'src/**/*.hs' -o output.css
```

Compiles CSS classes in those file paths or patterns, and writes `./tailwind.css`. Pass `-w` to run in JIT watcher mode.

Used in Emanote, and Ema apps.
