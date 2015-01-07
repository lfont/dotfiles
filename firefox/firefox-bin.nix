with import <nixpkgs> {};

{ version, url, sha256 } :

pkgs.lib.overrideDerivation pkgs.firefox-bin (attrs: rec {
    name  = "firefox-aurora-bin-${version}";

    src = fetchurl {
        url = url;
        sha256 = sha256;
    };

    installPhase = ''
        # This binary does not exists in aurora
        cp firefox mozilla-xremote-client
    '' + attrs.installPhase + ''
        rm "$out/bin/firefox"
        ln -s "$prefix/usr/lib/${attrs.name}/firefox" "$out/bin/firefox-aurora"

        rm $out/share/applications/firefox.desktop
        cat > $out/share/applications/firefox-aurora.desktop <<EOF
        [Desktop Entry]
        Type=Application
        Exec=$out/bin/firefox-aurora
        Icon=$out/usr/lib/${attrs.name}/browser/icons/mozicon128.png
        Name=Firefox Aurora
        GenericName=Web Browser
        Categories=Application;Network;
        EOF
    '';
})

