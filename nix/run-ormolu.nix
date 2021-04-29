{ writeShellScriptBin, ormolu, git, jq, yq }:

writeShellScriptBin "run-ormolu" ''
  set -e

  mode="$1"
  shift

  sourceFiles=("$@")

  if [ ! -z "$sourceFiles" ]; then
    num_files=$(wc -w <<< "''${sourceFiles[*]}")
    verb=$([[ "$mode" = 'check' ]] && echo "Checking" || echo "Formatting")
    echo "$verb $num_files files with ormolu..."

    # get the git rootDir in case somebody runs git commit or make format while not at the top level
    # in CI, git will fail because we dont have a .git, and so we default to '.'
    # note that an error message (not a git repository) is printed in CI, but is safe to ignore
    rootDir="$(${git}/bin/git rev-parse --show-toplevel || echo '.')"

    # dynamically generate list of extensions to enable by jq'ing package.yaml
    OLDIFS=$IFS
    IFS=$'\n' extensions=( $(cat $rootDir/package.yaml | ${yq}/bin/yq '."default-extensions"' | ${jq}/bin/jq '.[]' -r) )
    IFS=$OLDIFS

    ormoluCommand="${ormolu}/bin/ormolu "
    for ext in "''${extensions[@]}"; do
      ormoluCommand+="-o '-X$ext' "
    done
    ormoluCommand+="-m '$mode' ''${sourceFiles[*]}"

    # the following command will fail with code 100 if we are in "check" mode
    eval $ormoluCommand

    echo "done"
  fi
''
