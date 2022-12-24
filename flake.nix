{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixlib.url = "github:nix-community/nixpkgs.lib";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    digga = {
      url = "github:divnix/digga";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
        nixlib.follows = "nixlib";
        darwin.follows = "darwin";
      };
    };

    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
    nur-bandithedoge.url = "github:bandithedoge/nur-packages";
    nur-bandithedoge.flake = false;
    nur.url = "github:nix-community/NUR";
    parinfer-rust.url = "github:eraserhd/parinfer-rust";
    parinfer-rust.flake = false;
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
    colors.url = "github:Misterio77/nix-colors";
    nix-gaming.url = "github:fufexan/nix-gaming";
    prismlauncher.url = "github:PrismLauncher/PrismLauncher";

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
    musnix.url = "github:musnix/musnix";
    kmonad.url = "github:kmonad/kmonad?dir=nix";
    nixmox.url = "github:Sorixelle/nixmox";
  };

  outputs = {
    self,
    digga,
    nixpkgs,
    home-manager,
    musnix,
    kmonad,
    nixos-hardware,
    ...
  } @ inputs: let
    overlays = with inputs; [
      nur.overlay
      neovim-nightly-overlay.overlay
      nixmox.overlay
      nixpkgs-wayland.overlay
      mozilla.overlays.firefox
      nix-gaming.overlays.default
      prismlauncher.overlay
      (import (parinfer-rust + "/overlay.nix"))
      (_: prev: {
        bandithedoge = import nur-bandithedoge {pkgs = prev;};
        colors = colors.lib-core;
      })
      neorg.overlays.default
      (import ./overlay.nix)
    ];

    userProfiles = digga.lib.rakeLeaves ./users/profiles;
    userSuites = with userProfiles; {
      base = [
        core
        editors
      ];
      gui = [gui];
      gaming = [gaming];
      darwin = [os-specific.darwin];
      linux = [os-specific.linux];
    };
  in
    digga.lib.mkFlake {
      inherit self inputs;

      sharedOverlays = overlays;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
      };

      channels = {
        nixpkgs = {
          inherit overlays;
          input = nixpkgs;
        };
      };

      nixos = {
        hostDefaults = {
          system = "x86_64-linux";
          channelName = "nixpkgs";
          modules = [
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            musnix.nixosModules.musnix
            kmonad.nixosModules.default
            {
              home-manager.users.bandithedoge = {
                imports = with userSuites; linux;
              };
            }
          ];
        };

        imports = [(digga.lib.importHosts ./hosts/nixos)];

        importables = rec {
          profiles =
            digga.lib.rakeLeaves ./profiles
            // {
              users = digga.lib.rakeLeaves ./users;
            };
          suites = with profiles; {
            base = [core.nixos users.bandithedoge];
            gui = [gui];
            audio = [audio];
            gaming = [gaming];
            virt = [virt];
          };
        };

        hosts = {
          thonkpad = {
            modules = [
              nixos-hardware.nixosModules.lenovo-thinkpad-t440p
              nixos-hardware.nixosModules.common-pc-laptop-ssd
            ];
          };
          machine-nixos = {
            modules = with nixos-hardware.nixosModules; [
              common-pc
              common-pc-ssd
              common-cpu-intel-cpu-only
              common-gpu-amd
              {
                home-manager.users.bandithedoge = {
                  imports = with userSuites; gaming;
                };
              }
            ];
          };
        };
      };

      darwin = {
        hostDefaults = {
          system = "x86_64-darwin";
          channelName = "nixpkgs";
          modules = [
            digga.darwinModules.nixConfig
            home-manager.darwinModules.home-manager
            ./nix.nix
            {
              home-manager.users.bandithedoge = {
                imports = with userSuites; darwin;
              };
            }
          ];
        };

        imports = [(digga.lib.importHosts ./hosts/darwin)];

        importables = rec {
          profiles =
            digga.lib.rakeLeaves ./profiles
            // {
              users = digga.lib.rakeLeaves ./users;
            };
          suites = with profiles; {
            base = [core.darwin users.bandithedoge];
          };
        };
      };

      home = {
        modules = [];

        importables = {
          profiles = userProfiles;
          suites = userSuites;
        };

        users = {
          bandithedoge = {suites, ...}: {
            imports = with suites;
              base ++ gui;
          };
        };
      };

      # just wsl things
      homeConfigurations."bandithedoge" = let
        system = "x86_64-linux";
      in
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            {
              home = {
                homeDirectory = "/home/bandithedoge";
                username = "bandithedoge";
                stateVersion = "21.11";
              };
              nixpkgs = {inherit overlays;};
            }
            ./users/profiles/core
            ./users/profiles/editors
          ];
        };
    };
}
