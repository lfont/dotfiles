if [ "$(uname -s)" != "Darwin" ]
then
  exit 0
fi

# Check for Homebrew
if test ! $(which brew)
then
echo " Installing Homebrew for you."
  ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)" > $DOTFILES_TMP_DIR/homebrew-install.log
fi

exit 0

