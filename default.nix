{
  system ? builtins.currentSystem,
  compilerVersion ? "ghc8104",
  nixpkgs ? import ./nix/pinned-nixpkgs.nix
}:
let
  config = {
    packageOverrides = super:
    let self = super.pkgs; in rec {
      haskell-language-server = super.haskell-language-server // {
        override = { supportedGhcVersions = [ "8104" ]; };
      };
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compilerVersion}" = super.haskell.packages.${compilerVersion}.override {
            overrides = hself: hsuper: rec {
              haskwire =
                super.haskell.lib.overrideCabal
                ( hself.callCabal2nix "haskwire" ./. {})
                (drv: drv);
            };
          };
        };
      };
    };
  };
  pkgs = import nixpkgs { inherit config; inherit system; };
in
  {
    inherit pkgs;
    hsPkgs = pkgs.haskell.packages.${compilerVersion};
  }
