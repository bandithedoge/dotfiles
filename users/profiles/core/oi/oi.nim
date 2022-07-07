import std/[
  distros,
  os,
  osproc,
  strformat,
  strutils,
  terminal,
]
import docopt

proc exec(cmd: string) =
  styledEcho(styleBright, " ", cmd)

  let command = execCmd(cmd)

  if command != 0:
    styledEcho(fgRed, styleBright, $command, "  ", cmd)
    quit(command)
  else:
    styledEcho(fgGreen, styleBright, "")

when isMainModule:
  let
    doc = dedent &"""
      Usage:
        oi rebuild [-p=<path> (-t | --show-trace)]
        oi update [(-p=<path> | --path=<path>)] [<input> ...]
        oi cleanup
        oi (-h | --help)

      Options:
        -t --show-trace          Show stack trace
        -p=<path> --path=<path>  Flake path [default: {expandTilde "~/dotfiles"}]
    """
    args = docopt(doc)
    inputs = args["<input>"]
    path = args["--path"]
    showTrace = args["--show-trace"]
    # detectOs(NixOS) doesn't detect NixOS properly
    isNixOS = readFile("/etc/os-release").contains("NAME=NixOS")
    isDarwin = detectOs MacOSX
    username = getEnv("USER")

  if args["rebuild"]:
    let cmd =
      if isDarwin:
        &"darwin-rebuild --flake {path}"
      elif isNixOS:
        &"sudo nixos-rebuild --flake {path}"
      else:
        &"home-manager --extra-experimental-features 'nix-command flakes' --flake {path}#{username}"

    exec(
      # we need to use bash since sh doesn't support the "|&" syntax
      # "&>" doesn't work on Alpine for some reason
      "bash -c \"" &
      cmd &
      " switch --impure " &
      (if showTrace: "--show-trace " else: "") &
      "|& nom" &
      "\""
    )

  if args["update"]:
    if inputs.len() == 0:
      exec(&"nix flake update {path}")
      if isNixOS or isDarwin:
        exec("sudo nix-channel --update")
      else:
        exec("nix-channel --update")
    else:
      for input in inputs:
        exec(&"nix flake lock {path} --update-input {input}")

  if args["cleanup"]:
    exec(
      if isNixOS:
        "sudo nix-env --delete-generations +3"
      elif isDarwin:
        ""
      else:
        ""
    )
    exec(
      if isNixOS:
        "sudo nix-collect-garbage"
      else:
        "nix-collect-garbage"
    )
