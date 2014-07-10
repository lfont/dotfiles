with import <nixpkgs> {};

let
    version = "1.3.106";
    source = {
        arch = "linux-x64";
        sha256 = "c2f6efb35b420d1f436bfd17841321318ac94d3965b176f1b3bed5a9315cc7f1";
    };

in

stdenv.mkDerivation rec {
    name = "btsync-bin-${version}";

    src = fetchurl {
        url = "http://download-lb.utorrent.com/endpoint/btsync/os/linux-x64/track/stable";
        sha256 = source.sha256;
    };

    phases = "installPhase";

    libPath = stdenv.lib.makeLibraryPath
        [ stdenv.gcc.gcc
          glib
          glibc
        ] + ":" + stdenv.lib.makeSearchPath "lib64" [
            stdenv.gcc.gcc
        ];

    dontStrip = 1;

    installPhase = ''
        mkdir -p "$prefix/usr/lib/${name}"
        tar -C "$prefix/usr/lib/${name}/" -xzvf ${src}

        mkdir -p "$out/bin"
        ln -s "$prefix/usr/lib/${name}/btsync" "$out/bin/"

        for executable in \
            btsync
        do
            patchelf --interpreter "$(cat $NIX_GCC/nix-support/dynamic-linker)" \
                "$out/usr/lib/${name}/$executable"
        done

        for executable in \
            btsync
        do
            patchelf --set-rpath "$libPath" \
                "$out/usr/lib/${name}/$executable"
        done
    '';
}

