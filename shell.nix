with import ./default.nix {};
let
  refreshScript = pkgs.writeShellScriptBin "ref"
    ''
    hpack .
    cabal new-build
    '';
  ghcidScript = pkgs.writeShellScriptBin "dev"
    ''
    ${refreshScript}
    ghcid --command 'cabal new-repl lib:haskwire' --allow-eval --warnings -o ghcid.txt
    '';
  runScript = pkgs.writeShellScriptBin "run" "cabal run exe:demo";

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
      runScript
    ];
}

