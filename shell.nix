with import ./default.nix {};
let
  run-ormolu = pkgs.callPackage ./nix/run-ormolu.nix {};
  git-pre-commit = pkgs.callPackage ./nix/git-pre-commit-hook.nix {
    inherit (hsPkgs) hlint;
    inherit run-ormolu;
  };
  update-git-hook = pkgs.callPackage ./nix/update-git-hook.nix {};
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
  formatScript = pkgs.writeShellScriptBin "format" "${run-ormolu}/bin/run-ormolu --mode inplace $(find . -name '*.hs')";

in hsPkgs.shellFor {
    packages = myHsPkgs: [
      myHsPkgs.haskwire
    ];
    # withHoogle = true;
    buildInputs = with pkgs; [
      cabal-install # Cabal, haskell build tool
      cabal2nix # Utility to download Haskell packages into Nix format
      haskell-language-server # Language server
      hsPkgs.ghcid # Haskell repl with hot reloading
      hsPkgs.hpack # Generate cabal file from package.yaml
      hsPkgs.ormolu # Formatter
      hsPkgs.hlint # Linter

      # Demo dependencies
      yarn
      nodejs

      # Scripts
      refreshScript
      ghcidScript
      ghcidTestScript
      runScript
      runTestScript
      formatScript
    ];

    shellHook = ''
      source "${update-git-hook}/bin/haskwire-update-git-hook"
      updateGitHook "pre-commit" "${git-pre-commit}/bin/haskwire-git-pre-commit" "AlwaysRun"
    '';
}

