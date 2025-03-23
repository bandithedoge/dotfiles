{
  pkgs,
  config,
  ...
}: {
  programs = {
    regreet = with pkgs.rice; {
      enable = true;
      cageArgs = ["-s" "-d" "-m" "last"];
      inherit (gtk) theme iconTheme;
      cursorTheme = {
        inherit (cursorTheme) name package;
      };
      font = {
        package = uiFontPackage;
        name = uiFont;
        size = 12;
      };
      settings = {
        background = {
          path = wallpaperBlurred;
          fit = "Fill";
        };
        widget.clock.format = "%A %d %B %T";
      };
      extraCss = builtins.readFile (compileSCSS ../gtk.scss);
    };

    niri.enable = true;
    system-config-printer.enable = true;
  };

  services = {
    accounts-daemon.enable = true;
    gnome.gnome-keyring.enable = true;
    greetd.enable = true;
    flatpak.enable = true;
  };

  xdg.terminal-exec.enable = true;
  xdg.portal.extraPortals = with pkgs; [xdg-desktop-portal-gtk];

  gtk.iconCache.enable = true;

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      corefonts
      nerd-fonts.symbols-only
      rice.monoFontPackage
      rice.uiFontPackage
      roboto-slab
      twemoji-color-font
      vista-fonts
    ];
    fontconfig = {
      enable = true;
      # hinting.autohint = true;
      defaultFonts = with pkgs.rice; {
        monospace = [monoFont];
        sansSerif = [uiFont];
        serif = [serifFont];
      };
    };
  };

  boot = {
    extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
    kernelModules = ["v4l2loopback"];
    extraModprobeConfig = ''
      options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
    '';
    kernel.sysctl = {
      "vm.mmap_min_addr" = 0;
    };
  };

  security = {
    soteria.enable = true;
    pam = {
      services.gtklock = {};
      loginLimits = [
        {
          domain = "*";
          type = "hard";
          item = "nofile";
          value = "2097152";
        }
        {
          domain = "@realtime";
          type = "-";
          item = "memlock";
          value = "unlimited";
        }
      ];
    };
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
}
