with import <nixpkgs> {};

let
    subVersion = "33.0a1";
    source = {
        locale = "en-US";
        arch = "linux-x86_64";
        sha256 = "2abe001f3dfac4e4b18ae524164a105abdcf8fcb805d88fe9551fa0c7ae996f5";
    };

in

pkgs.lib.overrideDerivation pkgs.firefox-bin (attrs: rec {
    version = "nightly";

    name = "firefox-bin-${version}";

    src = fetchurl {
        url = "http://ftp.mozilla.org/pub/mozilla.org/firefox/${version}/latest-trunk/firefox-${subVersion}.${source.locale}.${source.arch}.tar.bz2";
        sha256 = source.sha256;
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

