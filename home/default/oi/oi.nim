import std/[
  distros,
  os,
  osproc,
  strformat,
  strutils,
  terminal,
  strtabs,
]
import docopt

proc exec(cmd: string) =
  styledEcho(styleBright, " ", cmd)

  let githubToken =
    if existsEnv("GITHUB_TOKEN"):
        getEnv("GITHUB_TOKEN", "")
    else:
      execProcess("gh auth token")

  putEnv("NIX_CONFIG", &"access-tokens = github.com={githubToken}")

  let command = execCmd(cmd)

  if command != 0:
    styledEcho(fgRed, styleBright, $command, "  ", cmd)
    quit command
  else:
    styledEcho(fgGreen, styleBright, "")

when isMainModule:
  let
    doc = dedent &"""
      Usage:
        oi (rebuild | r) [-p=<path> (-t | --show-trace)]
        oi (update | u) [(-p=<path> | --path=<path>)] [<input> ...]
        oi (cleanup | c)
        oi (-h | --help)

      Options:
        -t --show-trace          Show stack trace
        -p=<path> --path=<path>  Flake path [default: {expandTilde "~/dotfiles"}]
    """
    args = docopt doc
    inputs = args["<input>"]
    path = args["--path"]
    showTrace = args["--show-trace"]
    isDarwin = detectOs MacOSX
    isLinux = detectOs Linux
    # detectOs(NixOS) doesn't detect NixOS properly
    isNixOS = if isDarwin: false else: readFile(
        "/etc/os-release").contains "NAME=NixOS"
    username = getEnv "USER"

  if args["rebuild"] or args["r"]:
    let cmd =
      if isDarwin:
        &"darwin-rebuild --flake {path}"
      elif isNixOS:
        &"nixos-rebuild --flake {path}"
      else:
        &"home-manager --extra-experimental-features 'nix-command flakes' --flake {path}#{username}"

    let
      oldSystemPath = expandSymlink "/run/current-system/sw"
      oldHomePath = expandSymlink &"/etc/static/profiles/per-user/{username}"

    exec(
      # we need to use bash since sh doesn't support the "|&" syntax
      # "&>" doesn't work on Alpine for some reason
      (if isNixOS: "sudo " else: "") &
      &"bash -c \"{cmd} switch --impure " &
      (if showTrace: "--show-trace " else: "") &
      "|& nom\""
    )

    let
      newSystemPath = expandSymlink "/run/current-system/sw"
      newHomePath = expandSymlink &"/etc/static/profiles/per-user/{username}"

    if isNixOS:
      exec &"nvd diff {oldSystemPath} {newSystemPath}"

    exec &"nvd diff {oldHomePath} {newHomePath}"

  if args["update"] or args["u"]:
    if inputs.len() == 0:
      exec &"nix flake update {path}"
      exec (if isNixOS: "sudo " else: "") & "nix-channel --update"
      if isLinux:
        exec "flatpak update"
    else:
      for input in inputs:
        exec &"nix flake lock {path} --update-input {input}"

  if args["cleanup"] or args["c"]:
    exec "home-manager expire-generations '-3 days'"
    exec (if isNixOS or isDarwin: "sudo " else: "") & "nix-collect-garbage -d"
    exec (if isNixOS: "sudo " else: "") & "nix-store --optimise"
    if isDarwin: exec "brew cleanup -s --prune=all"
