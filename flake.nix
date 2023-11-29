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

    hyprland-split-monitor-workspaces = {
      inputs.hyprland.follows = "hyprland";
      url = "github:Duckonaut/split-monitor-workspaces";
    };
    hyprland.url = "github:hyprwm/Hyprland";
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    nil.url = "github:oxalica/nil";
    nix-alien.url = "github:thiagokokada/nix-alien";
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nixd.url = "github:nix-community/nixd";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nur.url = "github:nix-community/NUR";
    nyx.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    poetry2nix.url = "github:nix-community/poetry2nix";
    prismlauncher.url = "github:PrismLauncher/PrismLauncher";

    colors.url = "github:Misterio77/nix-colors";
    musnix.url = "github:musnix/musnix";
    kmonad.url = "github:kmonad/kmonad?dir=nix";
    kmonad.flake = false;
    nixmox.url = "github:Sorixelle/nixmox";
  };

  outputs = inputs @ {self, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "x86_64-darwin"];
      imports = with inputs; [nixos-flake.flakeModule];
      flake = {
        nixosConfigurations = let
          defaults = rec {
            overlays = with inputs; [
              (_: prev: {
                bandithedoge = import nur-bandithedoge {
                  pkgs = import nur-bandithedoge.inputs.nixpkgs {inherit (prev) system;};
                };
                colors = colors.lib-core;
                hyprlandPlugins = {
                  split-monitor-workspaces = hyprland-split-monitor-workspaces.packages.${prev.system}.default;
                };
              })
              mozilla.overlays.firefox
              neorg.overlays.default
              neovim.overlay
              nil.overlays.default
              nix-alien.overlays.default
              nix-gaming.overlays.default
              nixd.overlays.default
              nixmox.overlay
              nixpkgs-wayland.overlay
              nur.overlay
              poetry2nix.overlays.default
              prismlauncher.overlays.default
              (_: prev: {
                inherit (nix-gaming.packages.${prev.system}) wine-ge;
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
                self.nixosModules.default
                home-manager.nixosModules.home-manager
                hyprland.nixosModules.default
                musnix.nixosModules.musnix
                nyx.nixosModules.default
                "${kmonad}/nix/nixos-module.nix"
                self.nixosModules.home-manager
                ./users/bandithedoge.nix
                ./nix.nix
                ./nixos
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
                ./nixos/gaming.nix
                ./nixos/virt.nix
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
                ./nixos/virt.nix
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
            hyprland.homeManagerModules.default
            nix-index-database.hmModules.nix-index
          ];
        };
      };
    };
  #   nixos = {
  #     hostDefaults = {
  #       system = "x86_64-linux";
  #       channelName = "nixpkgs";
  #       modules = with inputs; [
  #         digga.nixosModules.nixConfig
  #         home-manager.nixosModules.home-manager
  #         hyprland.nixosModules.default
  #         musnix.nixosModules.musnix
  #         nyx.nixosModules.default
  #         "${kmonad}/nix/nixos-module.nix"
  #         {
  #           home-manager.users.bandithedoge = {
  #             imports = with userSuites; linux;
  #           };
  #         }
  #       ];
  #     };
  #
  #     imports = [(digga.lib.importHosts ./hosts/nixos)];
  #
  #     importables = rec {
  #       profiles =
  #         digga.lib.rakeLeaves ./profiles
  #         // {
  #           users = digga.lib.rakeLeaves ./users;
  #         };
  #       suites = with profiles; {
  #         base = [core.nixos users.bandithedoge];
  #         gui = [gui];
  #         audio = [audio];
  #         gaming = [gaming];
  #         virt = [virt];
  #       };
  #     };
  #
  #     hosts = {
  #       thonkpad = {
  #         modules = [
  #           nixos-hardware.nixosModules.lenovo-thinkpad-t440p
  #           nixos-hardware.nixosModules.common-pc-laptop-ssd
  #           {
  #             home-manager.users.bandithedoge = {
  #               imports = with userSuites; gaming-lite;
  #             };
  #           }
  #         ];
  #       };
  #       machine-nixos = {
  #         modules = with nixos-hardware.nixosModules; [
  #           {
  #             home-manager.users.bandithedoge = {
  #               imports = with userSuites; gaming;
  #             };
  #           }
  #         ];
  #       };
  #     };
  #   };
  #
  #   darwin = {
  #     hostDefaults = {
  #       system = "x86_64-darwin";
  #       channelName = "nixpkgs";
  #       modules = [
  #         digga.darwinModules.nixConfig
  #         home-manager.darwinModules.home-manager
  #         ./nix.nix
  #         {
  #           home-manager.users.bandithedoge = {
  #             imports = with userSuites; darwin;
  #           };
  #         }
  #       ];
  #     };
  #
  #     imports = [(digga.lib.importHosts ./hosts/darwin)];
  #
  #     importables = rec {
  #       profiles =
  #         digga.lib.rakeLeaves ./profiles
  #         // {
  #           users = digga.lib.rakeLeaves ./users;
  #         };
  #       suites = with profiles; {
  #         base = [core.darwin users.bandithedoge];
  #       };
  #     };
  #   };
  #
  #   home = {
  #     modules = with inputs; [
  #       hyprland.homeManagerModules.default
  #       nix-doom-emacs.hmModule
  #       nix-index-database.hmModules.nix-index
  #     ];
  #
  #     importables = {
  #       profiles = userProfiles;
  #       suites = userSuites;
  #     };
  #
  #     users = {
  #       bandithedoge = {suites, ...}: {
  #         imports = with suites;
  #           base ++ gui;
  #       };
  #     };
  #   };
  #
  #   # just wsl things
  #   nixosConfigurations.machine-windows = nixpkgs.lib.nixosSystem {
  #     system = "x86_64-linux";
  #     modules = with inputs; [
  #       home-manager.nixosModules.home-manager
  #       nixos-wsl.nixosModules.wsl
  #       ./nix.nix
  #       ./wsl.nix
  #       {
  #         home-manager = {
  #           useUserPackages = true;
  #           users.bandithedoge = {
  #             imports = [
  #               ./users/profiles/core/default.nix
  #               ./users/profiles/editors
  #               nix-index-database.hmModules.nix-index
  #             ];
  #             nixpkgs = {
  #               inherit overlays;
  #               config.allowUnfree = true;
  #             };
  #             home = {
  #               homeDirectory = "/home/bandithedoge";
  #               username = "bandithedoge";
  #             };
  #           };
  #         };
  #       }
  #     ];
  #   };
  #
  #   homeConfigurations.wsl = home-manager.lib.homeManagerConfiguration {
  #     pkgs = import nixpkgs {
  #       system = "x86_64-linux";
  #       config.allowUnfree = true;
  #     };
  #     modules = [
  #       {
  #         home = {
  #           homeDirectory = "/home/bandithedoge";
  #           username = "bandithedoge";
  #         };
  #       }
  #       ./users/profiles/core
  #       ./users/profiles/editors
  #     ];
  #   };
}
