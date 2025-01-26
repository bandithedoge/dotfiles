{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

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

    aagl.inputs.nixpkgs.follows = "nixpkgs";
    aagl.url = "github:ezKEa/aagl-gtk-on-nix";
    blink.url = "github:Saghen/blink.cmp";
    colors.url = "github:Misterio77/nix-colors";
    emacs.url = "github:nix-community/emacs-overlay";
    flatpak.url = "github:gmodena/nix-flatpak";
    ironbar.url = "github:JakeStanger/ironbar";
    matlab.url = "gitlab:doronbehar/nix-matlab";
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
    sops-nix.url = "github:Mic92/sops-nix";

    betterfox.url = "github:yokoffing/Betterfox";
    betterfox.flake = false;
  };

  outputs = inputs @ {self, ...}: let
    defaults = rec {
      overlays = with inputs; [
        emacs.overlays.default
        # lix-module.overlays.default
        matlab.overlay
        neorg.overlays.default
        neovim-plugins.overlays.default
        neovim.overlays.default
        nix-alien.overlays.default
        nix-gaming.overlays.default
        nur.overlays.default
        nyx.overlays.default
        poetry2nix.overlays.default

        (_: prev: {
          bandithedoge = nur-bandithedoge.legacyPackages.${prev.system};
          # bandithedoge = import nur-bandithedoge {
          #   pkgs = import nur-bandithedoge.inputs.nixpkgs {
          #     inherit (prev) system;
          #     config.allowUnfree = true;
          #   };
          # };

          vimPlugins =
            prev.vimPlugins
            // {
              inherit (blink.packages.${prev.system}) blink-cmp;
            };

          colors = colors.lib-core;
          inherit (aagl.packages.${prev.system}) honkers-railway-launcher anime-game-launcher;
          inherit (emacs.packages.${prev.system}) emacs-unstable-pgtk commercial-emacs;
          inherit (nix-gaming.packages.${prev.system}) wine-ge wine-tkg;
          # inherit (ironbar.packages.${prev.system}) ironbar;
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
      imports = with inputs; [nixos-unified.flakeModule];

      flake = {
        nixosModules.default = {lib, ...}: {
          imports = with inputs; [
            aagl.nixosModules.default
            flatpak.nixosModules.nix-flatpak
            home-manager.nixosModules.home-manager
            musnix.nixosModules.musnix
            nix-gaming.nixosModules.pipewireLowLatency
            nyx.nixosModules.default
            sops-nix.nixosModules.default

            ./nix.nix
            ./nixos
            ./sops.nix
            ./users/bandithedoge.nix
          ];
        };

        homeModules.default = {
          pkgs,
          lib,
          ...
        }: {
          imports = with inputs; [
            flatpak.homeManagerModules.nix-flatpak
            ironbar.homeManagerModules.default
            nix-index-database.hmModules.nix-index
            sops-nix.homeManagerModule

            ./home/default

            ./nix.nix
            ./sops.nix
          ];

          # config.nix.package = lib.mkDefault pkgs.lix;

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
                  # config.rocmSupport = true;
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
