{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    lix = {
      url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
      flake = false;
    };
    lix-module = {
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.lix.follows = "lix";
      url = "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixos-unified.url = "github:srid/nixos-unified";

    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin/master";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/22.05-5c211b47";

    nur-bandithedoge.url = "github:bandithedoge/nur-packages";
    # nur-bandithedoge.url = "path:/home/bandithedoge/git/nur-packages";

    aagl = {
      url = "github:ezKEa/aagl-gtk-on-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    astal = {
      url = "github:aylur/astal";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    blink = {
      url = "github:Saghen/blink.cmp";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    colors = {
      url = "github:Misterio77/nix-colors";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    flatpak.url = "github:gmodena/nix-flatpak";
    matlab.url = "gitlab:doronbehar/nix-matlab";
    musnix = {
      url = "github:musnix/musnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neorg = {
      url = "github:nvim-neorg/nixpkgs-neorg-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    neovim-plugins = {
      url = "github:m15a/flake-awesome-neovim-plugins";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    nyx.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    betterfox = {
      url = "github:yokoffing/Betterfox";
      flake = false;
    };
    kvlibadwaita = {
      url = "github:GabePoel/KvLibadwaita";
      flake = false;
    };
    firefox-ui-fix = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };
  };

  outputs = inputs @ {self, ...}: let
    defaults = rec {
      overlays = with inputs; [
        matlab.overlay
        neorg.overlays.default
        neovim-plugins.overlays.default
        nix-alien.overlays.default
        nix-gaming.overlays.default
        nur.overlays.default
        nyx.overlays.default
        poetry2nix.overlays.default

        (_: prev: {
          bandithedoge = nur-bandithedoge.legacyPackages.${prev.system};
          colors = colors.lib-core;

          vimPlugins = prev.vimPlugins // {inherit (blink.packages.${prev.system}) blink-cmp;};

          inherit (aagl.packages.${prev.system}) honkers-railway-launcher anime-game-launcher;
          inherit (nix-gaming.packages.${prev.system}) wine-ge wine-tkg;
          astal = astal.packages.${prev.system} // {inherit (astal) lib;};
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
    };
  in
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      debug = true;
      systems = ["x86_64-linux" "x86_64-darwin"];
      imports = with inputs; [
        nixos-unified.flakeModule
      ];

      flake = {
        nixosModules.default = {lib, ...}: {
          imports = with inputs; [
            {inherit (defaults) nixpkgs;}

            aagl.nixosModules.default
            flatpak.nixosModules.nix-flatpak
            home-manager.nixosModules.home-manager
            musnix.nixosModules.musnix
            nix-gaming.nixosModules.pipewireLowLatency
            nyx.nixosModules.default
            sops-nix.nixosModules.default

            {home-manager.useGlobalPkgs = lib.mkForce false;}

            ./nix.nix
            ./nixos
            ./sops.nix
            ./users/bandithedoge.nix
          ];
        };

        homeModules.default = {pkgs, ...}: {
          imports = with inputs; [
            {inherit (defaults) nixpkgs;}

            flatpak.homeManagerModules.nix-flatpak
            niri.homeModules.niri
            nix-index-database.hmModules.nix-index
            nyx.homeModules.default
            sops-nix.homeManagerModule

            ./home/default

            ./nix.nix
            ./sops.nix
          ];

          options.hostname = pkgs.lib.mkOption {type = pkgs.lib.types.str;};
        };

        nixosConfigurations = {
          thonkpad =
            self.nixos-unified.lib.mkLinuxSystem
            {
              home-manager = true;
            }
            {
              nixpkgs = defaults.nixpkgs // {hostPlatform = "x86_64-linux";};
              imports = with inputs.nixos-hardware.nixosModules; [
                self.nixosModules.default

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
                      ./home/hosts/thonkpad.nix
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
              imports = with inputs.nixos-hardware.nixosModules; [
                self.nixosModules.default

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
                      ./home/hosts/machine-nixos.nix
                      ./home/os-specific/linux
                    ];
                  };
                }
              ];
            };
        };
      };

      perSystem = {
        system,
        pkgs,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs (defaults.nixpkgs // {inherit system;});

        legacyPackages.homeConfigurations.bandithedoge = self.nixos-unified.lib.mkHomeConfiguration pkgs {
          imports = with inputs; [
            self.homeModules.default
            # lix-module.nixosModules.default
          ];

          inherit (defaults) nixpkgs;

          home = {
            username = "bandithedoge";
            stateVersion = "25.05";
            homeDirectory = "/${
              if pkgs.stdenv.isDarwin
              then "Users"
              else "home"
            }/bandithedoge";
          };
        };
      };
    };
}
