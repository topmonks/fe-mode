# IronMode

*This is work in progress. Use only for experiments and entertaiment.*

Lightweight FE library embracing the `view = f(data)` pattern.

`fe-mode.lit` is lieghtweight wrapper around [lit-html](https://lit-html.polymer-project.org/). It doesn't use 
vDOM diffing but native browser capabilities o top of `<template>` element and Shadow DOM.

`fe-mode.ql` is the QL part of [qlkit](https://github.com/forward-blockchain/qlkit) decoupled from React rendering. 
It uses metadata on functions to define queries instead of custom macro syntax.

The goal is to have high-performance & lightweight library for writing PWAs in ClojureScript. 
We aim to support Hiccup syntax on top of lit (next step) and then Server Side Rendering. 

## Local development and Testing

Run in terminal:

```bash
clojure -A:dev
open http://localhost:9500/figwheel-extra-main/auto-testing
```
