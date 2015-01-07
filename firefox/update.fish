if test ! (which nix-env)
    exit 0
end

. ./firefox/env.fish

set source_sha256 (nix-prefetch-url $URL)

#nix-build -K --argstr version $VERSION --argstr url $URL --argstr sha256 $source_sha256 ./firefox/firefox-bin.nix
nix-env -if ./firefox/firefox-bin.nix --argstr version $VERSION --argstr url $URL --argstr sha256 $source_sha256

exit 0

