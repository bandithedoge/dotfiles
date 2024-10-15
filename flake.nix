{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    lix.inputs.nixpkgs.follows = "nixpkgs";
    lix.url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.0.tar.gz";
    nixos-unified.url = "github:srid/nixos-unified";

    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin/master";
    home-manager.url = "github:nix-community/home-manager";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/22.05-5c211b47";

    nur-bandithedoge.url = "github:bandithedoge/nur-packages";
    # nur-bandithedoge.url = "path:/home/bandithedoge/git/nur-packages";

    aagl.url = "github:ezKEa/aagl-gtk-on-nix";
    colors.url = "github:Misterio77/nix-colors";
    emacs.url = "github:nix-community/emacs-overlay";
    flatpak.url = "github:gmodena/nix-flatpak";
    hypridle.url = "github:hyprwm/hypridle";
    matlab.url = "gitlab:doronbehar/nix-matlab";
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
    musnix.url = "github:musnix/musnix";
    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neovim-plugins.url = "github:m15a/flake-awesome-neovim-plugins";
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
  };

  outputs = inputs @ {self, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "x86_64-darwin"];
      imports = with inputs; [nixos-unified.flakeModule];
      flake = {
        nixosConfigurations = let
          defaults = rec {
            overlays = with inputs; [
              emacs.overlays.default
              lix.overlays.default
              matlab.overlay
              mozilla.overlays.firefox
              neorg.overlays.default
              neovim-plugins.overlays.default
              neovim.overlays.default
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
                  hyprsplit = hyprsplit.packages.${prev.system}.default;
                };
                inherit (aagl.packages.${prev.system}) honkers-railway-launcher anime-game-launcher;
                inherit (emacs.packages.${prev.system}) emacs-unstable-pgtk commercial-emacs;
                inherit (nix-gaming.packages.${prev.system}) wine-ge wine-tkg;
                inherit (prismlauncher.packages.${prev.system}) prismlauncher prismlauncher-unwrapped;
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
                flatpak.nixosModules.nix-flatpak
                home-manager.nixosModules.home-manager
                lix.nixosModules.default
                musnix.nixosModules.musnix
                nix-gaming.nixosModules.pipewireLowLatency
                nyx.nixosModules.default
                sops-nix.nixosModules.default

                ./nix.nix
                ./nixos
                ./sops.nix
                ./users/bandithedoge.nix
                self.nixosModules.default
              ];
            };
          };
        in {
          thonkpad =
            self.nixos-unified.lib.mkLinuxSystem
            {
              home-manager = true;
            }
            {
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

          machine-nixos =
            self.nixos-unified.lib.mkLinuxSystem
            {
              home-manager = true;
            }
            {
              nixpkgs =
                defaults.nixpkgs
                // {
                  hostPlatform = "x86_64-linux";
                  config.rocmSupport = true;
                };
              imports = with inputs.nixos-hardware.nixosModules;
                defaults.imports.nixos
                ++ [
                  common-pc
                  common-pc-ssd
                  common-cpu-intel
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

        nixosModules.default = _: {
          home-manager.extraSpecialArgs = {inherit inputs;};
        };

        homeModules.default = {pkgs, ...}: {
          options.hostname = pkgs.lib.mkOption {type = pkgs.lib.types.str;};
          imports = with inputs; [
            ./nix.nix
            ./sops.nix
            flatpak.homeManagerModules.nix-flatpak
            nix-index-database.hmModules.nix-index
            sops-nix.homeManagerModule
          ];
        };
      };
    };
}
