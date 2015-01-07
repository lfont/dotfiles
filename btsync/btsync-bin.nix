with import <nixpkgs> {};

let
    version = "1.4.106";
    source = {
        arch = "linux-x64";
        sha256 = "1prs3r6xcxq31xfdp3w2wdi3d7r6lw5r4d4zay2cwphmp4kpg3qg";
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

