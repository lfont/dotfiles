# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  time.timeZone = "Europe/Paris";

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";
  boot.kernelPackages = pkgs.linuxPackages_3_18;

  networking.hostName = "loics-laptop"; # Define your hostname.
  networking.hostId = "2d128811";
  # networking.wireless.enable = true;  # Enables wireless.
  networking.networkmanager.enable = true;
  networking.extraHosts = ''
     192.168.0.18 bibimbap
  '';

  # Select internationalisation properties.
  i18n = {
     consoleFont = "lat9w-16";
     consoleKeyMap = "us-acentos";
     defaultLocale = "en_US.UTF-8";
  };

  # Fonts
  fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
          dejavu_fonts
          inconsolata
      ];
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
     wget
     htop
     fish
     tmux
     psmisc
     which
     hdparm
     unzip

     gnupg
     gnupg1orig
     gnutls

     xclip
     xfontsel
     hsetroot
     xlibs.xev
     xlibs.xbacklight
     xlibs.xf86inputsynaptics
     xlibs.xkill
     glxinfo
     rxvt_unicode

     aspell
     aspellDicts.en
     aspellDicts.fr

     libnotify
     notify-osd
     
     stalonetray
     #clipit
     pasystray
     networkmanagerapplet

     xfce.xfconf
     xfce.xfce4_power_manager

     dmenu2
     xscreensaver
     nitrogen
     xarchiver
     pavucontrol
     bittorrentSync

     gtk # To get GTK+'s themes.
     gtk-engine-murrine
     hicolor_icon_theme
     gnome.gnomeicontheme
     #lxappearance

     haskellPackages.xmobar
     haskellPackages.ghc
     haskellPackages.xmonadContrib
     haskellPackages.xmonadExtras

     gnome.gnome_keyring
     polkit_gnome
     gvfs
     libfm
     #pcmanfm
     menu-cache
     #lxmenu-data
     desktop_file_utils
     shared_mime_info
     xdg-user-dirs
     xdg_utils
  ];

  environment.variables.GIO_EXTRA_MODULES = [
     "${pkgs.gvfs}/lib/gio/modules"
  ];

  environment.shells = [
     "/run/current-system/sw/bin/fish"
  ];

  environment.pathsToLink = [
     "/share/themes"
     "/share/mime"
     "/share/desktop-directories"
  ];

  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = "powersave";

  hardware.enableAllFirmware = true;
  hardware.pulseaudio.enable = true;

  security.polkit.extraConfig = ''
    polkit.addRule(function (action, subject) {
      if ((action.id === 'org.freedesktop.policykit.exec' ||
           action.id === 'org.freedesktop.systemd1.manage-units') &&
          subject.local &&
          subject.isInGroup('wheel')) {
        return polkit.Result.YES;
      }
    });
  '';

  # List services that you want to enable:

  services.upower.enable = true;
  services.upower.package = pkgs.upower-old;
  services.udev.packages = [
      pkgs.gvfs
      pkgs.libmtp
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.autorun = true;
  services.xserver.vaapiDrivers = [ pkgs.vaapiIntel ];
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e, ctrl:nocaps";
  services.xserver.xkbVariant = "intl";
  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    buttonsMap = [ 1 3 2 ];
    tapButtons = false;
    palmDetect = true;
    additionalOptions = ''
      Option "FingerHigh"       "55"
      Option "FingerLow"        "50"
      Option "VertHysteresis"   "0"
      Option "HorizHysteresis"  "0"
      Option "VertScrollDelta"  "-111"
      Option "HorizScrollDelta" "-111"
    '';
  };

  # Enable the xmonad session.
  services.xserver.displayManager.slim.enable = true;
  services.xserver.displayManager.slim.defaultUser = "loic";

  services.xserver.desktopManager.default = "none";
  services.xserver.desktopManager.xterm.enable = false;

  services.xserver.windowManager.default = "none";
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.loic = {
     isNormalUser = true;
     uid = 1000;
     name = "loic";
     description = "Loïc Fontaine";
     extraGroups = [ "wheel" "networkmanager" ];
     useDefaultShell = false;
     shell = "/run/current-system/sw/bin/fish";
  };

  nixpkgs.config.allowUnfree = true;
  #nixpkgs.config.packageOverrides = pkgs: {
  #  upower = (pkgs.lib.overrideDerivation pkgs.upower (attrs: rec {
  #    name = "upower-0.9.23";
  #    src = pkgs.fetchurl {
  #      url = "http://upower.freedesktop.org/releases/${name}.tar.xz";
  #      sha256 = "06wqhab2mn0j4biiwh7mn4kxbxnfnzjkxvhpgvnlpaz9m2q54cj3";
  #    };
  #  }));
  #};
}
