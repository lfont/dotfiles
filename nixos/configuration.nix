# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  hardware.pulseaudio.enable = true;

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.kernelPackages = pkgs.linuxPackages_3_14;
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2
  '';

  networking.hostName = "loics-macbook-pro"; # Define your hostname.
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

  # List packages installed in system profile. To search by name, run:
  # -env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    htop
    fish
    tmux

    networkmanagerapplet
    gvfs
    xdg-user-dirs
    unzipNLS
    xarchiver
    gtk-engine-murrine

    vimHugeX

    git
    kde4.kdiff3

    python
    nodejs

    chromiumDev
    pidgin

    keepassx2

    dropbox

    shotwell

    bitcoin
    electrum
  ];

  environment.shells = [
    "/run/current-system/sw/bin/fish"
  ];

  # There is no way to define better sudo rules in 14.04
  # This is needed for the keyboard-backlight script
  security.sudo.wheelNeedsPassword = false;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.autorun = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "";
  services.xserver.xkbVariant = "intl";

  # Enable the Desktop Environment.
  services.xserver.displayManager.slim.enable = true;
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.twoFingerScroll = true;
  services.xserver.synaptics.buttonsMap = [ 1 3 2 ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.loic = {
    name = "loic";
    description = "Loïc Fontaine";
    group = "users";
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
    createHome = true;
    home = "/home/loic";
    shell = "/run/current-system/sw/bin/fish";
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.chromium.enablePepperFlash = true;
  nixpkgs.config.chromium.enablePepperPDF = true;
  nixpkgs.config.packageOverrides = pkgs: rec {
    # new kdiff3 version
    kde4 = {
        qt4 = pkgs.kde4.qt4;
        kdiff3 = (pkgs.lib.overrideDerivation pkgs.kde4.kdiff3 (attrs: rec {
            name = "kdiff3-0.9.98";
            src = pkgs.fetchurl {
                url = "mirror://sourceforge/kdiff3/${name}.tar.gz";
                sha256 = "0s6n1whkf5ck2r8782a9l8b736cj2p05and1vjjh7d02pax1lb40";
            };
        }));
    };
    # smb support
    gvfs = pkgs.gvfs.override { lightWeight = false; };
    # svg support
    xfce = {
     libxfce4util = pkgs.xfce.libxfce4util;
     xinitrc = pkgs.xfce.xinitrc;
     xfce4_power_manager = pkgs.xfce.xfce4_power_manager;
     xfce4notifyd = pkgs.xfce.xfce4notifyd;
     tumbler = (pkgs.lib.overrideDerivation pkgs.xfce.tumbler (attrs: {
        buildInputs = attrs.buildInputs ++ (with pkgs; [ makeWrapper ]);
        librsvg = pkgs.librsvg;
        preFixup = ''
          cat "$librsvg/lib/gdk-pixbuf/loaders.cache" >> "$GDK_PIXBUF_MODULE_FILE"

          wrapProgram "$out/lib/tumbler-1/tumblerd" \
            --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
        '';
     }));
     xfce4_appfinder = pkgs.xfce.xfce4_appfinder;
     thunar_volman = pkgs.xfce.thunar_volman;
     garcon = pkgs.xfce.garcon;
     libxfce4ui = pkgs.xfce.libxfce4ui;
     xfwm4 = pkgs.xfce.xfwm4;
     xfdesktop = (pkgs.lib.overrideDerivation pkgs.xfce.xfdesktop (attrs: {
       buildInputs = attrs.buildInputs ++ (with pkgs; [ gdk_pixbuf makeWrapper ]);
       librsvg = pkgs.librsvg;
       preFixup = ''
         cat "$librsvg/lib/gdk-pixbuf/loaders.cache" >> "$GDK_PIXBUF_MODULE_FILE"

         wrapProgram "$out/bin/xfdesktop" \
           --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
       '' + attrs.preFixup;
     }));
     xfconf = pkgs.xfce.xfconf;
     xfce4screenshooter = pkgs.xfce.xfce4screenshooter;
     xfce4mixer = pkgs.xfce.xfce4mixer;
     xfce4settings = pkgs.xfce.xfce4settings;
     xfce4session = (pkgs.lib.overrideDerivation pkgs.xfce.xfce4session (attrs: {
       buildInputs = attrs.buildInputs ++ (with pkgs; [ gdk_pixbuf makeWrapper ]);
       librsvg = pkgs.librsvg;
       preFixup = ''
        cat "$librsvg/lib/gdk-pixbuf/loaders.cache" >> "$GDK_PIXBUF_MODULE_FILE"

        wrapProgram "$out/bin/xfce4-session" \
          --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
       '' + attrs.preFixup;
     }));
     xfce4icontheme = pkgs.xfce.xfce4icontheme;
     thunar = (pkgs.lib.overrideDerivation pkgs.xfce.thunar (attrs: {
       buildInputs = attrs.buildInputs ++ (with pkgs; [ gdk_pixbuf makeWrapper ]);
       librsvg = pkgs.librsvg;
       preFixup = ''
         cat "$librsvg/lib/gdk-pixbuf/loaders.cache" >> "$GDK_PIXBUF_MODULE_FILE"

         wrapProgram "$out/bin/thunar" \
           --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
       '' + attrs.preFixup;
     }));
     terminal = pkgs.xfce.terminal;
     ristretto = (pkgs.lib.overrideDerivation pkgs.xfce.ristretto (attrs: {
       buildInputs = attrs.buildInputs ++ (with pkgs; [ gdk_pixbuf makeWrapper ]);
       librsvg = pkgs.librsvg;
       preFixup = ''
         cat "$librsvg/lib/gdk-pixbuf/loaders.cache" >> "$GDK_PIXBUF_MODULE_FILE"

         wrapProgram "$out/bin/ristretto" \
           --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
       '' + attrs.preFixup;
     }));
     mousepad = pkgs.xfce.mousepad;
     libxfcegui4 = pkgs.xfce.libxfcegui4;
     gtk_xfce_engine = pkgs.xfce.gtk_xfce_engine;
     exo = pkgs.xfce.exo;
     gvfs = gvfs;
     xfce4panel = (pkgs.lib.overrideDerivation pkgs.xfce.xfce4panel (attrs: {
       buildInputs = attrs.buildInputs ++ (with pkgs; [ gdk_pixbuf makeWrapper ]);
       librsvg = pkgs.librsvg;
       preFixup = ''
         cat "$librsvg/lib/gdk-pixbuf/loaders.cache" >> "$GDK_PIXBUF_MODULE_FILE"

         wrapProgram "$out/bin/xfce4-panel" \
           --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
       '' + attrs.preFixup;
      }));
    };
  };
}

