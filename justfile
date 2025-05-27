#!/usr/bin/env -S just --working-directory ${HOME}/dotfiles --justfile

export GITHUB_TOKEN := `gh auth token`
token-opt := "--option access-tokens github.com=${GITHUB_TOKEN}"

default:
    nh os switch . -- {{ token-opt }}

update +INPUTS="":
    nix flake update {{ INPUTS }} {{ token-opt }}

    @if [[ "{{ INPUTS }}" == "" ]]; then flatpak update; flatpak uninstall --unused; fi

clean:
    nh clean all -v
