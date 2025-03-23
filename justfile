#!/usr/bin/env -S just --working-directory ${HOME}/dotfiles --justfile

export GITHUB_TOKEN := `gh auth token`
token-opt := "--option access-tokens github.com=${GITHUB_TOKEN}"

default:
    nh os switch . -- {{ token-opt }}

update +INPUTS="":
    nix flake update {{ INPUTS }} {{ token-opt }}

    @if [[ "{{ INPUTS }}" == "" ]]; then nix-channel --update; flatpak update; flatpak uninstall --unused; fi

    @if [[ "{{ INPUTS }}" == *"home-manager"* ]] || [[ "{{ INPUTS }}" == "" ]]; then home-manager news --flake .#bandithedoge; fi

clean:
    nh clean all -v
