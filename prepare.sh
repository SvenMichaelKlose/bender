#! /bin/sh

echo "Installing required packages for bender with aptitude…"
sudo aptitude install git sbcl
echo "Cloning, building and installing git repositories required for bender…"
git clone https://github.com/svenmichaelklose/tre --depth 1
cd tre; sh make.sh core: sh make.sh install
