cd ~/bin && { curl -OL http://sourceforge.net/projects/kpcli/files/kpcli-2.8.pl; cd -; }

chmod +x ~/bin/kpcli-2.8.pl

cpan -T \
    File::KeePass \
    Term::ShellUI \
    Term::ReadKey \
    Term::ReadLine \
    Capture::Tiny \
    Clipboard \
    Data::Password \
    Sub::Install \
    Term::ReadLine::Gnu \
    Sort::Naturally \
    Crypt::Rijndael

PATCH=$(pwd)/kpcli/Xclip.patch
cd ~/perl5/lib/perl5/Clipboard && { patch < $PATCH; cd -; }
