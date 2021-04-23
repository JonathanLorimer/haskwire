{
  system ? builtins.currentSystem,
  compilerVersion ? "ghc8104",
  nixpkgs ? import ./nix/pinned-nixpkgs.nix
}:

let
  ssl-proxy-overlay = self: _: {
    ssl-proxy = self.buildGoModule {
      pname = "ssl-proxy";
      version = "0.2.6";
      src = self.fetchFromGitHub {
        owner = "suyashkumar";
        repo = "ssl-proxy";
        rev = "v0.2.6";
        sha256 = "1z1bmc37qpybb4dc9ayqfhvq4si4yf8vnric0zxlnha2f80647i7";
      };
      modSha256 = "0s2k0jiap76l39njxqh3vlkl2y8wh3xk4939zawh8yk2vsyssp8c";
      vendorSha256 = "0s2k0jiap76l39njxqh3vlkl2y8wh3xk4939zawh8yk2vsyssp8c";
    };
  };
  overlays = [
    ssl-proxy-overlay
  ];
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
  pkgs = import nixpkgs { inherit config; inherit system; inherit overlays; };
in
  {
    inherit pkgs;
    hsPkgs = pkgs.haskell.packages.${compilerVersion};
  }
