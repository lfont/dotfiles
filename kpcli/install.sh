. ./script/functions.sh

VERSION=3.0
SHA256=947b7b0485215f72e14bb8936c847abb583253c597f58234650922270259049c

file_path=$(f_install_file http://sourceforge.net/projects/kpcli/files/kpcli-$VERSION.pl $SHA256)

if [[ ! -e $file_path ]]; then
    echo 'Remote file is invalid'
    exit 1
fi

chmod +x $file_path
f_link_command $file_path kpcli.pl

PERL_PACKAGES="Crypt::Rijndael
               Term::ReadKey
               Sort::Naturally
               File::KeePass
               Term::ShellUI
               Term::ReadLine::Gnu
               Clipboard
               Capture::Tiny
               Data::Password
               Clone
               XML::Parser
               Term::ReadLine
               Term::ReadKey
               Sub::Install"

for p in $PERL_PACKAGES; do
    perl -MCPAN -e "notest install '$p'"
done

PATCH=$(pwd)/kpcli/Xclip.patch
cd ~/perl5/lib/perl5/Clipboard && { patch < $PATCH; cd -; }
