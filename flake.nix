{
  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://kira-bruneau.cachix.org"
      "https://bandithedoge.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      "kira-bruneau.cachix.org-1:FJSccwNPRNHPBHN+qxAme2Svp537q7dDuHkqLnyOTaQ="
      "bandithedoge.cachix.org-1:ZtcHw1anyEa4t6H8m3o/ctYFrwYFPAwoENSvofamE6g="
    ];
    extra-experimental-features = "nix-command flakes";
  };
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

    nur.url = "github:nix-community/NUR";
    nur-bandithedoge.url = "github:bandithedoge/nur-packages";
    nur-bandithedoge.flake = false;
    nur-bandithedoge.inputs.nixpkgs.follows = "nixpkgs";
    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neorg.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
    parinfer-rust.url = "github:eraserhd/parinfer-rust";
    parinfer-rust.flake = false;

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
    musnix.url = "github:musnix/musnix";
    kmonad.url = "github:kmonad/kmonad?dir=nix";
    nixgl.url = "github:guibou/nixGL";
    firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
  };

  outputs = {
    self,
    digga,
    darwin,
    nixpkgs,
    home-manager,
    nixos-hardware,
    musnix,
    kmonad,
    ...
  } @ inputs:
    digga.lib.mkFlake {
      inherit self inputs;

      sharedOverlays = with inputs; [
        nur.overlay
        neovim-nightly-overlay.overlay
        nixgl.overlay
        neorg.overlay
        # nixpkgs-wayland.overlay
        (import (parinfer-rust + "/overlay.nix"))
        (final: prev: {
          bandithedoge = import nur-bandithedoge {pkgs = prev;};
        })
        (import ./overlay.nix)
      ];

      channelsConfig.allowUnfree = true;
      channels = {
        nixpkgs = {};
      };

      nixos = {
        hostDefaults = {
          system = "x86_64-linux";
          channelName = "nixpkgs";
          modules = [
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            musnix.nixosModules.musnix
            kmonad.nixosModule
          ];
        };

        imports = [(digga.lib.importHosts ./hosts)];

        importables = rec {
          profiles =
            digga.lib.rakeLeaves ./profiles
            // {
              users = digga.lib.rakeLeaves ./users;
            };
          suites = with profiles; {
            base = [core users.bandithedoge];
            gui = [gui];
            audio = [audio];
          };
        };

        hosts = {
          thonkpad = {};
        };
      };

      home = {
        modules = [];

        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/profiles;
          suites = with profiles; {
            base = [
              core
              editors
            ];
            linux = [os-specific.linux];
            gui = [
              wayland
              x
            ];
            audio = [
              audio
            ];
          };
        };

        users = {
          bandithedoge = {suites, ...}: {
            imports = with suites;
              base
              ++ gui
              ++ audio
              ++ linux;
          };
        };
      };

      homeConfigurations = digga.lib.mkHomeConfigurations self.nixosConfigurations;
    };
  #
  # let
  #   hmModule = {
  #     home-manager = {
  #       useGlobalPkgs = true;
  #       useUserPackages = true;
  #       users.bandithedoge = import ./home;
  #     };
  #   };
  #
  #   common = {
  #     imports = [ ./common ];
  #     nix.registry = with inputs; { nixpkgs.flake = nixpkgs; };
  #   };
  #
  #   nixpkgsConfig = {
  #     nixpkgs = {
  #       overlays = with inputs; [
  #         neovim-nightly-overlay.overlay
  #         nixgl.overlay
  #         vim-extra-plugins.overlay
  #         neorg.overlay
  #         (import (parinfer-rust + "/overlay.nix"))
  #         (import (nur-bandithedoge + "/overlay.nix"))
  #         (import ./overlay.nix)
  #       ];
  #       config.packageOverrides = pkgs: {
  #         nur = import inputs.nur { inherit pkgs; };
  #       };
  #     };
  #   };
  #
  # in
  # {
  #   darwinConfigurations."machine" = darwin.lib.darwinSystem {
  #     system = "x86_64-darwin";
  #     modules = [
  #       ./common
  #       ./darwin
  #       nixpkgsConfig
  #       home-manager.darwinModule
  #       (nixpkgs.lib.mkMerge [
  #         hmModule
  #         { home-manager.users.bandithedoge.imports = [ ./home/darwin ]; }
  #       ])
  #     ];
  #   };
  #   nixosConfigurations.thonkpad = nixpkgs.lib.nixosSystem {
  #     system = "x86_64-linux";
  #     extraArgs = { inherit inputs; };
  #     modules = [
  #       ./common
  #       ./nixos
  #       nixpkgsConfig
  #       nixos-hardware.nixosModules.lenovo-thinkpad-t440p
  #       musnix.nixosModules.musnix
  #       home-manager.nixosModules.home-manager
  #       kmonad.nixosModule
  #       (nixpkgs.lib.mkMerge [
  #         hmModule
  #         { home-manager.users.bandithedoge.imports = [ ./home/linux ]; }
  #       ])
  #     ];
  #   };
  #   homeConfigurations.bandithedoge = home-manager.lib.homeManagerConfiguration {
  #     configuration = {
  #       imports = [ ./home ./home/wsl ];
  #     } // nixpkgsConfig;
  #     system = "x86_64-linux";
  #     username = "bandithedoge";
  #     homeDirectory = "/mnt/c/Users/bandithedoge";
  #   };
  # };
}
