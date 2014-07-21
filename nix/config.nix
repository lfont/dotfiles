{
  allowUnfree = true;

  chromium.enableGoogleTalkPlugin = true;
  chromium.enablePepperFlash = true;

  packageOverrides = pkgs: rec {
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
  };
}

