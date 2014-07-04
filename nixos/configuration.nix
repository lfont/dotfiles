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
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2
  '';
  boot.postBootCommands = ''
    echo 0 > /sys/class/leds/smc::kbd_backlight/brightness
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
    gnome3.gtk
    gtk-engine-murrine

    vimHugeX
    git

    nodejs
    python

    firefox
    pidgin
    keepassx2
    dropbox
    bitcoin
  ];

  environment.shells = [
    "/run/current-system/sw/bin/fish"
  ];

  # There is no way to define better sudo rules in 14.04
  # This is needed for the keyboard-backlight script
  security.sudo.wheelNeedsPassword = false;

  # List services that you want to enable:

  services.acpid.enable = true;

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
  nixpkgs.config.packageOverrides = pkgs: {
    # smb support
    gvfs = pkgs.gvfs.override { lightWeight = false; };
    # svg support
   # xfce.xfce4panel = (pkgs.lib.overrideDerivation pkgs.xfce.xfce4panel (attrs: {
   #     buildInputs = [
   #         pkgconfig intltool gtk xfce.libxfce4util xfce.exo libwnck
   #         xfce.garcon xfce.xfconf libstartup_notification
   #         gdk_pixbuf librsvg
   #     ];

   #     preFixup = ''
   #         wrapProgram "$out/bin/xfce4-panel" \*/
   #             --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
   #         rm $out/share/icons/hicolor/icon-theme.cache
   #     '';
   # }));
  };
}

