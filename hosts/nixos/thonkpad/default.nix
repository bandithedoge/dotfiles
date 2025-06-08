{pkgs, ...}: {
  environment = {
    systemPackages = with pkgs; [
      connman-gtk
    ];
    variables = {
      BROWSER = "qutebrowser";
    };
  };

  boot = {
    kernelModules = ["kvm-intel"];
    initrd.availableKernelModules = [
      "ahci"
      "ehci_pci"
      "rtsx_pci_sdmmc"
      "sd_mod"
      "sr_mod"
      "usb_storage"
      "xhci_pci"
    ];
    kernel.sysctl = {
      "rcutree.enable_rcu_lazy" = 1;
    };
    loader.grub = {
      device = "nodev";
    };
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = pkgs.lib.mkForce "conservative";
    powerUpCommands = ''
      ${pkgs.kmod}/bin/modprobe -r psmouse
      ${pkgs.kmod}/bin/modprobe psmouse
    '';
    powertop.enable = true;
  };
  services = {
    kanata = {
      # {{{
      enable = true;
      keyboards.internal = {
        devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
        config = builtins.readFile ./kmonad.kbd;
      };
    }; # }}}

    libinput = {
      enable = true;
      touchpad.tapping = false;
    };

    acpid.enable = true;
    thermald.enable = true;
    tlp.enable = true;
    logind.lidSwitch = "suspend";
    connman.enable = true;
  };

  hardware.firmware = with pkgs; [
    linux-firmware
  ];

  hardware.graphics.enable = true;

  # drives {{{
  disko.devices.disk.nixos = {
    type = "disk";
    device = "/dev/sda";
    name = "nixos";
    content = {
      type = "gpt";
      partitions = {
        boot = {
	  size = "512M";
	  type = "EF00";
	  content = {
	    type = "filesystem";
	    format = "vfat";
	    mountpoint = "/boot";
	    mountOptions = ["umask=0077"];
	  };
	};

	root = {
	  size = "100%";
	  content = {
	    type = "luks";
	    name = "root";
	    passwordFile = "/tmp/secret.key";
	    settings.allowDiscards = true;
	    content = {
	      type = "btrfs";
	      extraArgs = ["-f"];
	      subvolumes = let
	      in {
	        "/root" = {
		      mountpoint = "/";
		    };
		    "/home" = {
		      mountpoint = "/home";
	          mountOptions = [
		        "compress=zstd"
		      ];
		    };
		    "/nix" = {
		      mountpoint = "/nix";
	          mountOptions = [
		        "compress=zstd"
		        "noatime"
		      ];
		    };
		    "/swap" = {
		      mountpoint = "/.swapvol";
		      swap.swapfile.size = "4G";
		    };
	      };
	    };
	  };
	};
      };
    };
  };
  # }}}

  networking.hostName = "thonkpad";

  networking.wireless.allowAuxiliaryImperativeNetworks = true;

  # jebać ose
  security.pki.certificateFiles = [
    (pkgs.fetchurl {
      url = "https://ose.gov.pl/media/2022/09/certyfikat-OSE.crt";
      sha256 = "0Du2OmOnvCzaGz3ZHu7KsnL23WjDxNfgn3VFqzf2ffA=";
    })
  ];
}
