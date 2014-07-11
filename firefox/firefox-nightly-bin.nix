with import <nixpkgs> {};

{ version, sha256 } :

let
    locale = "en-US";
    arch = "linux-x86_64";

in

pkgs.lib.overrideDerivation pkgs.firefox-bin (attrs: rec {
    name = "firefox-nightly-bin-${version}";

    src = fetchurl {
        url = "http://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-trunk/firefox-${version}.${locale}.${arch}.tar.bz2";
        sha256 = sha256;
    };

    installPhase = attrs.installPhase + ''
        rm "$out/bin/firefox"
        ln -s "$prefix/usr/lib/${attrs.name}/firefox" "$out/bin/firefox-nightly"

        rm $out/share/applications/firefox.desktop
        cat > $out/share/applications/firefox-nightly.desktop <<EOF
        [Desktop Entry]
        Type=Application
        Exec=$out/bin/firefox-nightly
        Icon=$out/usr/lib/${attrs.name}/browser/icons/mozicon128.png
        Name=Firefox Nightly
        GenericName=Web Browser
        Categories=Application;Network;
        EOF
    '';
})

