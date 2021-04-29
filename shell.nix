with import ./default.nix {};
let
  refreshScript = pkgs.writeShellScriptBin "ref"
    ''
    hpack .
    cabal new-build
    '';
  ghcidScript = pkgs.writeShellScriptBin "dev"
    ''
    hpack .
    cabal new-build
    ghcid --command 'cabal new-repl lib:haskwire' --allow-eval --warnings -o ghcid.txt
    '';
  ghcidTestScript = pkgs.writeShellScriptBin "dev-test"
    ''
    hpack .
    cabal new-build
    ghcid --command 'cabal new-repl test:test' --allow-eval --warnings -o ghcid.txt
    '';
  runScript = pkgs.writeShellScriptBin "run" "cabal run exe:demo";
  runTestScript = pkgs.writeShellScriptBin "run-test" "cabal run test:test";
  formatScript = pkgs.writeShellScriptBin "format" "ormolu --mode inplace $(find . -name '*.hs')";

in hsPkgs.shellFor {
    packages = myHsPkgs: [
      myHsPkgs.haskwire
    ];
    # withHoogle = true;
    buildInputs = with pkgs; [
      cabal-install # cabal, haskell build tool
      cabal2nix # Utility to download Haskell packages into Nix format
      haskell-language-server # language server
      hsPkgs.ghcid # haskell repl with hot reloading
      hsPkgs.hpack # generate cabal file from package.yaml
      hsPkgs.ormolu # linter

      # Demo dependencies
      yarn
      nodejs

      # Scripts
      refreshScript
      ghcidScript
      ghcidTestScript
      runScript
      runTestScript
      proxyScript
      formatScript
    ];
}

