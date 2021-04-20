builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  url = "https://github.com/nixos/nixpkgs/";
  # Commit hash for nixos-unstable as of 2018-09-12
  # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
  ref = "refs/heads/nixos-unstable";
  rev = "f5e8bdd07d1afaabf6b37afc5497b1e498b8046f";
}
