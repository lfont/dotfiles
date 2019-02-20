# https://nixos.wiki/wiki/FAQ#Why_not_use_nix-env_-i_foo.3F
# bootstrap with: nix-env -f '<nixpkgs>' -r -iA userPackages
self: super: {
    userPackages = super.userPackages or {} // {
        ## helper script to keep user env clean
        nix-rebuild = super.writeScriptBin "nix-rebuild"
        ''
            #!${super.stdenv.shell}
            exec nix-env -f '<nixpkgs>' -r -iA userPackages
        '';

        ## nix (nscd, https://github.com/NixOS/nix/issues/599)
        nix = super.nix;
        nixops = super.nixops;
        glibcLocales = super.glibcLocales;

        ## common packages
        firefox = super.firefox;
        syncthing = super.syncthing;
        tree = super.tree;
        pass = super.pass;
	mosh = super.mosh;
	gnupg = super.gnupg;
	pcmanfm = super.pcmanfm;
        curl = super.curl;
        git = super.git;
	clipit = super.clipit;
	pinentry_qt5 = super.pinentry_qt5;

        ## wine
	wine = super.wine;
	winetricks = super.winetricks;

        ## emacs
        emacs = super.emacs;
        xclip = super.xclip;
        multimarkdown = super.multimarkdown;

        ## themes (applied with dconf)
        arc-theme = super.arc-theme;
        arc-icon-theme = super.arc-icon-theme;
        paper-icon-theme = super.paper-icon-theme;
	# .overrideAttrs (oldAttrs: {
        #     # https://github.com/NixOS/nixpkgs/issues/22652
        #     postFixup =
        #     ''
        #         mkdir -p $out/share/icons/default
        #         cat << EOF > $out/share/icons/default/index.theme
        #         [Icon Theme]
        #         Name=Default
        #         Comment=Default Cursor Theme
        #         Inherits=Paper
        #         EOF
        #     '' + oldAttrs.postFixup;
        # });

        ## wm
        # i3 = super.i3;
        # i3status = super.i3status;
        # slock = super.slock; # need setuid
        dmenu = super.dmenu;
        # nodejs = super.nodejs-6_x;
        # xfce4_power_manager = super.xfce.xfce4_power_manager_gtk3; # https://forum.xfce.org/viewtopic.php?id=11190
        gnome-settings-daemon = super.gnome3.gnome-settings-daemon;
        gnome-tweak-tool = super.gnome3.gnome-tweak-tool;
        # polkit_gnome = super.polkit_gnome;
        wmctrl = super.wmctrl;
	stalonetray = super.stalonetray;
        # dunst = super.dunst;
        feh = super.feh;
        # cwm = super.cwm.overrideAttrs (oldAttrs: {
        #     name = "cwm-6.3";
        #     src = super.fetchFromGitHub {
        #         owner = "chneukirchen";
        #         repo = "cwm";
        #         rev = "a9dbac8209d0b5efa3bd5af0c287039545639117";
        #         sha256 = "1m08gd6nscwfx6040zbg2zl89m4g73im68iflzcihd6pdc8rzzs4";
        #     };
        # });
        # tint2 = super.tint2.overrideAttrs (oldAttrs: rec {
        #     name = "tint2-${version}";
        #     version = "16.4";
        #     src = super.fetchFromGitLab {
        #         owner = "o9000";
        #         repo = "tint2";
        #         rev = version;
        #         sha256 = "1h9l45zimai2hqfcf2y98g4i03imhmvm3mlsld9x99i650kxr5jm";
        #     };
        # });
        ifstat = super.ifstat-legacy;
        # xdotool = super.xdotool;
        st = super.st;
	xmonad = super.haskellPackages.ghcWithPackages(self: [
	  self.xmonad self.xmonad-contrib self.xmonad-extras self.xmobar
	]);

        ## beets
        beets = super.beets;
        pygobject2 = super.python27Packages.pygobject2;

        ## pulseaudio
        pavucontrol = super.pavucontrol;
        # pasystray = super.pasystray;

        ## daw
        # ardour = super.ardour;
        # a2jmidid = super.a2jmidid;
        # jack2 = super.jack2Full;
        # qjackctl = super.qjackctl;
        # calf = super.calf;
        # zynaddsubfx = super.zynaddsubfx;

        ## android
	androidsdk = super.androidsdk;
     };
}
