{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    nixos-flake.url = "github:srid/nixos-flake";

    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin/master";
    home-manager.url = "github:nix-community/home-manager";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/22.05-5c211b47";

    nur-bandithedoge.url = "github:bandithedoge/nur-packages";
    # nur-bandithedoge.url = "path:/home/bandithedoge/git/nur-packages";

    aagl.url = "github:ezKEa/aagl-gtk-on-nix";
    emacs.url = "github:nix-community/emacs-overlay";
    hypridle.url = "github:hyprwm/hypridle";
    hyprland-split-monitor-workspaces.inputs.hyprland.follows = "hyprland";
    hyprland-split-monitor-workspaces.url = "github:Duckonaut/split-monitor-workspaces";
    hyprland.url = "github:hyprwm/Hyprland";
    hyprlock.url = "github:hyprwm/hyprlock";
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    nix-alien.url = "github:thiagokokada/nix-alien";
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nur.url = "github:nix-community/NUR";
    nyx.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    poetry2nix.url = "github:nix-community/poetry2nix";
    prismlauncher.url = "github:PrismLauncher/PrismLauncher";
    sops-nix.url = "github:Mic92/sops-nix";
    zjstatus.url = "github:dj95/zjstatus";

    colors.url = "github:Misterio77/nix-colors";
    musnix.url = "github:musnix/musnix";
  };

  outputs = inputs @ {self, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "x86_64-darwin"];
      imports = with inputs; [nixos-flake.flakeModule];
      flake = {
        nixosConfigurations = let
          defaults = rec {
            overlays = with inputs; [
              emacs.overlays.default
              mozilla.overlays.firefox
              neorg.overlays.default
              neovim.overlay
              nix-alien.overlays.default
              nix-gaming.overlays.default
              nur.overlay
              poetry2nix.overlays.default
              prismlauncher.overlays.default

              (_: prev: {
                bandithedoge = import nur-bandithedoge {
                  pkgs = import nur-bandithedoge.inputs.nixpkgs {
                    inherit (prev) system;
                    config.allowUnfree = true;
                  };
                };
                colors = colors.lib-core;
                hyprlandPlugins = {
                  split-monitor-workspaces = hyprland-split-monitor-workspaces.packages.${prev.system}.default;
                };
                inherit (nix-gaming.packages.${prev.system}) wine-ge wine-tkg;
                inherit (prismlauncher.packages.${prev.system}) prismlauncher prismlauncher-unwrapped;
                inherit (emacs.packages.${prev.system}) emacs-unstable-pgtk commercial-emacs;
                inherit (aagl.packages.${prev.system}) honkers-railway-launcher;
              })
              (import ./overlay.nix)
            ];
            nixpkgs = {
              inherit overlays;
              config = {
                allowUnfree = true;
                allowBroken = true;
              };
            };
            imports = {
              nixos = with inputs; [
                aagl.nixosModules.default
                home-manager.nixosModules.home-manager
                hyprland.nixosModules.default
                musnix.nixosModules.musnix
                nix-gaming.nixosModules.pipewireLowLatency
                nyx.nixosModules.default
                sops-nix.nixosModules.default

                ./nix.nix
                ./nixos
                ./sops.nix
                ./users/bandithedoge.nix
                self.nixosModules.default
                self.nixosModules.home-manager
              ];
            };
          };
        in {
          thonkpad = self.nixos-flake.lib.mkLinuxSystem {
            nixpkgs = defaults.nixpkgs // {hostPlatform = "x86_64-linux";};
            imports = with inputs.nixos-hardware.nixosModules;
              defaults.imports.nixos
              ++ [
                lenovo-thinkpad-t440p
                common-pc-laptop-ssd
                ./hosts/nixos/thonkpad
                ./nixos/audio.nix
                ./nixos/gui.nix
                {
                  home-manager.users.bandithedoge = {
                    hostname = "thonkpad";
                    imports = [
                      self.homeModules.default
                      ./home/editors
                      ./home/gaming
                      ./home/gui
                      ./home/os-specific/linux
                    ];
                  };
                }
              ];
          };

          machine-nixos = self.nixos-flake.lib.mkLinuxSystem {
            nixpkgs = defaults.nixpkgs // {hostPlatform = "x86_64-linux";};
            imports = with inputs.nixos-hardware.nixosModules;
              defaults.imports.nixos
              ++ [
                common-pc
                common-pc-ssd
                common-cpu-intel-cpu-only
                common-gpu-amd
                ./hosts/nixos/machine-nixos
                ./nixos/audio.nix
                ./nixos/gui.nix
                ./nixos/gaming.nix
                {
                  home-manager.users.bandithedoge = {
                    hostname = "machine-nixos";
                    imports = [
                      self.homeModules.default
                      ./home/editors
                      ./home/gaming
                      ./home/gui
                      ./home/os-specific/linux
                    ];
                  };
                }
              ];
          };
        };

        nixosModules.default = {...}: {
          home-manager.extraSpecialArgs = {inherit inputs;};
        };

        homeModules.default = {pkgs, ...}: {
          options.hostname = pkgs.lib.mkOption {type = pkgs.lib.types.str;};
          imports = with inputs; [
            ./nix.nix
            ./sops.nix
            hypridle.homeManagerModules.default
            hyprland.homeManagerModules.default
            hyprlock.homeManagerModules.default
            nix-index-database.hmModules.nix-index
            sops-nix.homeManagerModule
          ];
        };
      };
    };
}
