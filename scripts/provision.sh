#!/bin/bash

# For Ubuntu deep learning base AMI 5.0
# Ubuntu 16.04, CUDA version 9.0.
# Used on a p3.2xlarge (one Tesla V100).

# terminate if any command fails
set -e

# First: set up custom dotfiles
git clone https://github.com/julianmichael/dotfiles.git
# Move last several PATH lines of ~/.bashrc to ~/.path, to get CUDA stuff on the PATH.
tail -n7 ~/.bashrc >> ~/dotfiles/files/path
rm ~/.bashrc
cd ~/dotfiles
./install
cd ~

# install system deps
sudo apt-get install tree # this is just for me
sudo add-apt-repository ppa:deadsnakes/ppa # need for installing python 3.6 with apt-get
sudo apt-get update
sudo apt-get install python3.6 # default python3 is 3.6; I install this for the nice venv functionality
sudo apt-get install python3.6-venv
sudo apt-get install python3.6-dev # needed for pip install spacy==2.0, at least
# sudo apt-get install libevent-dev # doesn't seem necessary, though some people said they needed it

# get the repository and initialize allennlp submodule
git clone https://github.com/julianmichael/qfirst.git
pushd ~/qfirst
./setup.sh

source env/bin/activate
pip uninstall torch
pip install http://download.pytorch.org/whl/cu90/torch-0.4.0-cp36-cp36m-linux_x86_64.whl
pip install torchvision

pushd env/lib/python3.6/site-packages/allennlp/custom_extensions
./make.sh
popd

popd
