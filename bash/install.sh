ln -sf $(pwd)/bash/bashrc  ~/.bashrc
ln -sf $(pwd)/bash/profile ~/.profile

sudo cp $(pwd)/bash/bashrc  /root/.bashrc
sudo cp $(pwd)/bash/profile /root/.profile

exit 0
