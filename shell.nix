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
  proxyScript = pkgs.writeShellScriptBin "proxy" ''
      PIDFILE="${toString ./tmp/proxy/ssl-proxy.pid}"
      CERT="${toString ./crt-file.crt}"
      KEY="${toString ./key-file.key}"
      die() {
        echo "Error: $2" >&2
        exit "$1"
      }
      [ "$USER" != "root" ] && die 1 "Must be run as root"
      start() {
        [ -f "$PIDFILE" ] && [ "$(ps -p $(<"$PIDFILE") -o comm=)" == "ssl-proxy" ] && \
          die 2 "Proxy already running"
        mkdir -p "$(dirname "$PIDFILE")"
        ${pkgs.ssl-proxy}/bin/ssl-proxy \
          -cert $CERT \
          -key $KEY \
          -redirectHTTP \
          -from 0.0.0.0:443 \
          -to 127.0.0.1:8081 &
        echo $! >"$PIDFILE"
      }
      stop() {
        [ ! -f "$PIDFILE" ] && die 3 "No running proxy"
        kill $(<"$PIDFILE")
        rm "$PIDFILE"
      }
      case $1 in
        start) start ;;
        stop) stop ;;
        *) echo "Usage: proxy (start|stop)" ;;
      esac
    '';
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

