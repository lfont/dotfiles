# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.grub.enable = false;
  boot.loader.grub.device = "/dev/sda4";
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;

  networking.hostName = "loics-macbook-pro"; # Define your hostname.
  networking.networkmanager.enable = true;
  networking.extraHosts = 
    ''
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
  nixpkgs.config.packageOverrides = pkgs: {
    gvfs = pkgs.gvfs.override { lightWeight = false; };
  };
}
