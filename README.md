# tailwind

Run TailwindCSS CLI without needing to touch JavaScript. No `input.css` or `tailwind.config.js` necessary.

```
cabal run tailwind-run -- 'src/**/.hs'
```

Compiles CSS classes in those file pattersn, and writes `./tailwind.css`. Pass `-w` to run in JIT watcher mode.