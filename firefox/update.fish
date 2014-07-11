if test ! (which nix-env)
    exit 0
end

set source_version "33.0a1"
set source_url http://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-trunk/firefox-{$source_version}.en-US.linux-x86_64.tar.bz2
set source_sha256 (curl $source_url | sha256sum | sed 's/  -$//')

nix-env -if ./firefox/firefox-nightly-bin.nix --argstr version $source_version --argstr sha256 $source_sha256

exit 0

