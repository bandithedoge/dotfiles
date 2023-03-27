{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixlib.url = "github:nix-community/nixpkgs.lib";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    digga = {
      url = "github:divnix/digga";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
        nixlib.follows = "nixlib";
        darwin.follows = "darwin";
      };
    };

    nixos-wsl.url = "github:nix-community/NixOS-WSL";

    nur-bandithedoge = {
      flake = false;
      url = "github:bandithedoge/nur-packages";
    };
    hyprland.url = "github:hyprwm/Hyprland";
    hyprpaper.url = "github:hyprwm/hyprpaper";
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    nil.url = "github:oxalica/nil";
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nur.url = "github:nix-community/NUR";
    prismlauncher.url = "github:PrismLauncher/PrismLauncher";

    colors.url = "github:Misterio77/nix-colors";
    musnix.url = "github:musnix/musnix";
    kmonad.url = "github:kmonad/kmonad";
    kmonad.flake = false;
    nixmox.url = "github:Sorixelle/nixmox";
  };

  outputs = {
    self,
    digga,
    nixpkgs,
    home-manager,
    nixos-hardware,
    ...
  } @ inputs: let
    overlays = with inputs; [
      hyprland.overlays.default
      hyprpaper.overlays.default
      mozilla.overlays.firefox
      neorg.overlays.default
      neovim.overlay
      nil.overlays.default
      nix-gaming.overlays.default
      nixmox.overlay
      nixpkgs-wayland.overlay
      nur.overlay
      prismlauncher.overlays.default
      (_: prev: {
        bandithedoge = import nur-bandithedoge {pkgs = prev;};
        colors = colors.lib-core;
      })
      (import ./overlay.nix)
    ];

    userProfiles = digga.lib.rakeLeaves ./users/profiles;
    userSuites = with userProfiles; {
      base = [
        core
        editors
      ];
      gui = [gui];
      gaming = [gaming gaming-lite];
      gaming-lite = [gaming-lite];
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
          modules = with inputs; [
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            musnix.nixosModules.musnix
            "${kmonad}/nix/nixos-module.nix"
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
              {
                home-manager.users.bandithedoge = {
                  imports = with userSuites; gaming-lite;
                };
              }
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
        modules = with inputs; [
          hyprland.homeManagerModules.default
          nix-index-database.hmModules.nix-index
        ];

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
      nixosConfigurations.wsl = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = with inputs; [
          home-manager.nixosModules.home-manager
          nixos-wsl.nixosModules.wsl
          ./nix.nix
          ./wsl.nix
          {
            home-manager = {
              useUserPackages = true;
              users.bandithedoge = {
                imports = [
                  ./users/profiles/core/default.nix
                  ./users/profiles/editors
                  nix-index-database.hmModules.nix-index
                ];
                nixpkgs = {inherit overlays;};
                home = {
                  homeDirectory = "/home/bandithedoge";
                  username = "bandithedoge";
                };
              };
            };
          }
        ];
      };
    };
}
