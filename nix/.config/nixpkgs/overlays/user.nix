# https://nixos.wiki/wiki/FAQ#Why_not_use_nix-env_-i_foo.3F
# bootstrap with: nix-env -f '<nixpkgs>' -r -iA userPackages
self: super: {
    userPackages = super.userPackages or {} // {
        # helper script to keep user env clean
        nix-rebuild = super.writeScriptBin "nix-rebuild"
        ''
            #!${super.stdenv.shell}
            exec nix-env -f '<nixpkgs>' -r -iA userPackages
        '';
        # nix (nscd, https://github.com/NixOS/nix/issues/599)
        nix = super.nix;
        nixops = super.nixops;
        glibcLocales = super.glibcLocales;
        # common packages
        firefox = super.firefox;
        syncthing = super.syncthing;
        tree = super.tree;
        pass = super.pass;
        # emacs
        emacs = super.emacs;
        xclip = super.xclip;
        multimarkdown = super.multimarkdown;
        # themes (applied with dconf)
        arc-theme = super.arc-theme;
        arc-icon-theme = super.arc-icon-theme;
        paper-icon-theme = super.paper-icon-theme.overrideAttrs (oldAttrs: {
            # https://github.com/NixOS/nixpkgs/issues/22652
            postFixup =
            ''
                mkdir -p $out/share/icons/default
                cat << EOF > $out/share/icons/default/index.theme
                [Icon Theme]
                Name=Default
                Comment=Default Cursor Theme
                Inherits=Paper
                EOF
            '' + oldAttrs.postFixup;
        });
        # i3
        i3 = super.i3;
        i3status = super.i3status;
        i3lock = super.i3lock; # pam issues
        dmenu = super.dmenu;
        nodejs = super.nodejs-6_x;
        xfce4_power_manager_gtk3 = super.xfce.xfce4_power_manager_gtk3; # https://forum.xfce.org/viewtopic.php?id=11190
        # beets
        beets = super.beets;
        pygobject2 = super.python27Packages.pygobject2;
        # pulseaudio
        pavucontrol = super.pavucontrol;
        pasystray = super.pasystray;
        # daw
        ardour = super.ardour;
        a2jmidid = super.a2jmidid;
        jack2 = super.jack2Full;
        qjackctl = super.qjackctl;
        qsynth = super.qsynth;
        calf = super.calf;
        zynaddsubfx = super.zynaddsubfx;
     };
}
