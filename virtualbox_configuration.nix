# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.checkJournalingFS = false;

  networking.hostName = "Nixpix"; # Define your hostname.
  networking.hostId = "8ef5ad7b";
  # networking.wireless.enable = true;  # Enables wireless.

  fileSystems."/vboxshare" = {
    fsType = "vboxsf";
    device = "vboxshare";
    options = "rw";
  };

  i18n = {
    consoleFont = "lat9w-16";
    # consoleKeyMap = "sv";
    defaultLocale = "en_US.utf-8";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
     emacs
     kbd
     zsh
     git
     gitAndTools.git-extras
     nox # search for nix packages
     chromium
     wget
     fasd # replacement for autojump (z) and other tools
     gnupg
     ruby # For something in my emacs

     python27Packages.virtualenvwrapper
     python27Packages.ipython
     python27Packages.elpy # for emacs
     python27Packages.rope # for emacs
     python27Packages.autopep8
     python27Packages.pyflakes
     python27Packages.setuptools
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.virtualboxGuest.enable = true;  

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "se";
  services.xserver.xkbOptions = "eurosign:e";

  services.xserver.displayManager.auto.enable = true;
  services.xserver.displayManager.auto.user = "gurgeh";


  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  services.xserver.desktopManager.kde4.enable = true;
  # services.xserver.desktopManager.kde5.enable = true;

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.gurgeh = {
    isNormalUser = true;
    home = "/home/gurgeh";
    extraGroups = ["wheel"];
    uid = 1000;
  };

}
