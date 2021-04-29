<p align="center">
  <img src="https://user-images.githubusercontent.com/32466011/115424637-e1492680-a1cc-11eb-8b76-66bff0d9567e.png" alt="Logo" width="320" height="320">

  <h2 align="center">Haskwire</h2>
  <strong align="center" style="display:block;">HTML Over The Wire, bringing SSR to Haskell</strong>
  <p align="center">🚧 Project under development 🚧</p>
</p>

## ⚙️ Setup

### 🛠️ Development

This repo depends on nix, and uses `nix-shell` to setup a dev environment. To setup nix on your setup please visit the [getting nix page](https://nixos.org/download.html). This repo uses cabal to build the haskell dependencies, but all of the dependencies are provided via the nix-shell.

There are several utility scripts provided by the nix-shell:
  - `ref` - This rebuilds the dependencies using cabal, you will have to run this after editing the `package.yaml` file.
  - `dev` - Runs `ghcid` for the haskwire library
  - `dev-test` - Runs `ghcid` for the test suite
  - `run` - Runs the demo server
  - `run-test` - Executes the test suite
  - `format` - Formats all hs files with `ormolu`

The nix shell also provides these haskell tools:
  - cabal
  - haskell-language-server
  - ghcid
  - ormolu

A typical workflow might look like this:

```bash
nix-shell
dev
```

### ✨ Running the demo app

nix-shell should also provide the correct version of `node` for you, and `yarn`. We just need to install the turbo client js and es-build to bundle our javascript.

```bash
nix-shell
cd demo/hotwire-client
yarn
yarn build
cd ../..
run
```

you should see this output in your terminal:

```bash
λ🔌 Running haskwire demo on port 8081 ✨✨✨
```

## 📍 Roadmap
  - Core Library
    - [x] Turbo
      - [x] Drive (fully client side)
      - [x] Frames (mostly client side)
      - [x] Streams
          - [ ] Bonus: provide utility for establishing websockets or SSE from the server
      - [ ] Native
      - [ ] Bonus: provide an index.js and html header snippet
    - [ ] Stimulus
      - [ ] Controllers
      - [ ] Actions
      - [ ] Targets
    - [ ] Strada (coming soon)
  - Demo
    - [ ] Scotty
    - [ ] Servant
    - [ ] Yesod
    - [ ] Trasa
  - README
    - [x] Roadmap
    - [ ] Docs
    - [ ] Tutorial
  - Framework - Provide a batteries included framework for a hotwire server
    - Server
    - Streaming (WS or SSE)
    - Templating
    - CDN?
    - CSS Framework?
