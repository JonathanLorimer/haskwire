# Exposes a shell function for updating git hooks.
#
# Usage: updateGitHook hookName hookCommand hookMode
#   hookName: a valid git hook name such as "pre-commit"
#   hookCommand: the command or file that will be run by git.
#                best practice is to pass a path to a shell script.
#   hookMode: one of ("AlwaysRun" "AlwaysRunWithNixShell" "SkipOutsideOfNixShell")
#             AlwaysRun: always runs the hookCommand
#             AlwaysRunWithNixShell: always runs the hookCommand, spawning a new a nix-shell
#             SkipOutsideOfNixShell: skips the hookCommand if not already inside a nix-shell
#
# Setup:
# 1. Import up the update-git-hook script
#    > let update-git-hook = pkgs.callPackage ./.nix/update-git-hook.nix {};
# 2. Source it in your shellHook to make the updateGitHook function available
#    > shellHook = ''
#        source "${update-git-hook}/bin/haskwire-update-git-hook"
#        ... call updateGitHook here
#      ''
#
# Examples:
# 1. Run the /tmp/foo script after every git checkout
#    > updateGitHook "post-checkout" "/tmp/foo" "AlwaysRun"
#
# 2. Run the /tmp/foo script after every git checkout, but make sure its run in the nix-shell
#    > updateGitHook "post-checkout" "/tmp/foo" "AlwaysRunWithNixShell"
#
# 3. Only run the /tmp/foo script after a git checkout in a nix-shell, skip it otherwise
#    > updateGitHook "post-checkout" "/tmp/foo" "SkipOutsideOfNixShell"
#
# 4. Run a nix-ified script as part of a git hook
#    > let git-post-checkout = pkgs.callPackage ./.nix/git-hooks/post-checkout.nix {};
#    > in shellHook = ''
#    >      source "${update-git-hook}/bin/haskwire-update-git-hook"
#    >      updateGitHook "post-checkout" "${git-post-checkout}/bin/haskwire-git-post-checkout" "SkipOutsideOfNixShell"
#    >   ''
{ writeShellScriptBin, gnused, git }:

writeShellScriptBin "haskwire-update-git-hook" ''
  updateGitHook () {
    local validHooks=(
      "applypatch-msg"
      "pre-applypatch"
      "post-applypatch"
      "pre-commit"
      "prepare-commit-msg"
      "commit-msg"
      "post-commit"
      "pre-rebase"
      "post-checkout"
      "post-merge"
      "pre-merge-commit"
      "pre-receive"
      "update"
      "post-receive"
      "post-update"
      "pre-auto-gc"
      "post-rewrite"
      "pre-push"
    )
    local validHookModes=("AlwaysRun" "AlwaysRunWithNixShell" "SkipOutsideOfNixShell")

    local hookName="$1"
    local hookCommand="$2"
    local hookMode="$3"

    if [[ "$#" -ne 3 ]]; then
      echo "ERROR: (updateGitHook) expected three arguments but got ''${#}"
      exit 1
    fi

    if [[ ! " ''${validHooks[@]} " =~ " ''${hookName} " ]]; then
      echo "ERROR: (updateGitHook) invalid hookName $hookName, must be one of ''${validHooks[*]}"
      exit 1
    fi

    if [[ ! " ''${validHookModes[@]} " =~ " ''${hookMode} " ]]; then
      echo "ERROR: (updateGitHook) invalid hookMode $hookMode, must be one of ''${validHookModes[*]}"
      exit 1
    fi

    local rootDir="$(${git}/bin/git rev-parse --show-toplevel)"
    local hookMarker="$(basename ""$hookCommand"")"
    local hookBegin="begin $hookMarker"
    local hookEnd="end $hookMarker"
    local gitHookPath="$(${git}/bin/git rev-parse --git-path hooks)/$hookName"
    local shellNixPath="$rootDir/shell.nix"

    touch $gitHookPath
    chmod u+x $gitHookPath

    # delete existing nix-hook
    ${gnused}/bin/sed -i "/# begin-haskwire-git-hook/,/# end-haskwire-git-hook/d" $gitHookPath # TODO: delete this line
    ${gnused}/bin/sed -i "/# $hookBegin/,/# $hookEnd/d" $gitHookPath

    if [[ "$hookMode" = "AlwaysRun" ]]; then
      cat << EOF1 >> $gitHookPath
  # $hookBegin
  $hookCommand \$@
  # $hookEnd
  EOF1
    elif [[ "$hookMode" = "AlwaysRunWithNixShell" ]]; then
      cat << EOF2 >> $gitHookPath
  # $hookBegin
  if [[ -z "\$IN_NIX_SHELL" ]]; then
    hookParams="\$@"
    nix-shell $shellNixPath --command "$hookCommand \$hookParams"
  else
    $hookCommand \$@
  fi
  # $hookEnd
  EOF2
    elif [[ "$hookMode" = "SkipOutsideOfNixShell" ]]; then
      cat << EOF3 >> $gitHookPath
  # $hookBegin
  if [[ -z "\$IN_NIX_SHELL" ]]; then
    echo "Not in nix-shell, skipping git $hookName hook."
  else
    $hookCommand \$@
  fi
  # $hookEnd
  EOF3
    fi
  }
''
