{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-parts.url = "github:hercules-ci/flake-parts";

    nixos-hardware.url = "github:nixos/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl.url = "github:nix-community/NixOS-WSL/22.05-5c211b47";
    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur-bandithedoge.url = "github:bandithedoge/nur-packages";

    aagl = {
      url = "github:ezKEa/aagl-gtk-on-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    colors = {
      url = "github:Misterio77/nix-colors";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    flatpak.url = "github:gmodena/nix-flatpak";
    musnix = {
      url = "github:musnix/musnix";
      inputs.nixpkgs.follows = "nixpkgs";
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
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nix-index-database.follows = "nix-index-database";
      };
    };
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nyx.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
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

  outputs =
    inputs@{
      self,
      flake-parts,
      nixpkgs,
      home-manager,
      nixos-hardware,
      ...
    }:
    let
      defaults = rec {
        overlays = with inputs; [
          neovim-plugins.overlays.default
          nix-alien.overlays.default
          nix-gaming.overlays.default
          nyx.overlays.default

          (_: prev: {
            bandithedoge = nur-bandithedoge.legacyPackages.${prev.system};
            colors = colors.lib-core;

            inherit (nix-gaming.packages.${prev.system}) wine-ge wine-tkg;
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
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
      ];
      imports = [ home-manager.flakeModules.home-manager ];

      flake = {
        nixosModules.default =
          { lib, ... }:
          {
            imports = with inputs; [
              { inherit (defaults) nixpkgs; }

              aagl.nixosModules.default
              disko.nixosModules.default
              flatpak.nixosModules.nix-flatpak
              home-manager.nixosModules.home-manager
              musnix.nixosModules.musnix
              nix-gaming.nixosModules.ntsync
              nix-gaming.nixosModules.pipewireLowLatency
              nyx.nixosModules.default
              sops-nix.nixosModules.default

              {
                home-manager = {
                  useGlobalPkgs = lib.mkForce false;
                  extraSpecialArgs = { inherit inputs; };
                };
              }

              ./nix.nix
              ./nixos
              ./sops.nix
              ./users/bandithedoge.nix
            ];
          };

        homeModules.default =
          { pkgs, ... }:
          {
            imports = with inputs; [
              { inherit (defaults) nixpkgs; }

              flatpak.homeManagerModules.nix-flatpak
              niri.homeModules.niri
              nix-index-database.hmModules.nix-index
              nyx.homeModules.default
              sops-nix.homeManagerModule

              ./home/default

              ./nix.nix
              ./sops.nix
            ];

            options.hostname = pkgs.lib.mkOption { type = pkgs.lib.types.str; };
          };

        nixosConfigurations = {
          thonkpad = nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs; };
            modules = with nixos-hardware.nixosModules; [
              self.nixosModules.default

              { nixpkgs.hostPlatform = "x86_64-linux"; }

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

          machine-nixos = nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs; };
            modules = with nixos-hardware.nixosModules; [
              self.nixosModules.default

              {
                nixpkgs = {
                  hostPlatform = "x86_64-linux";
                  config.rocmSupport = true;
                };
              }

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
    };
}
