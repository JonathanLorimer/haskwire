{ writeShellScriptBin, ormolu, git, hlint }:

writeShellScriptBin "haskwire-git-pre-commit" ''
  set -e

  IFS=$'\n' stagedFiles=("$(git diff --cached --name-only --diff-filter=d "*.hs")")

  if [ ! -z "$stagedFiles" ]; then
    ${ormolu} inplace "''${stagedFiles[*]}"

    if [ ! -z $MWB_HLINT_ON_PRECOMMIT ]; then
      echo "''${stagedFiles[*]}" | xargs ${hlint}/bin/hlint -j4
    fi

    ${git}/bin/git add ''${stagedFiles[*]}
  fi
''
