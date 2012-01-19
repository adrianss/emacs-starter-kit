#!/bin/sh
# Install rinari module an submodules
git submodule init vendor/rinari
git submodule update vendor/rinari

# rinari submodules
cd vendor/rinari
git submodule init util/jump
git submodule update util/jump
cd ../..

# Install yaml submodule
git submodule init vendor/yaml
git submodule update vendor/yaml

# Install feature submodule
git submodule init vendor/feature
git submodule update vendor/feature

# Install yasnippet submodule
git submodule init vendor/yasnippet
git submodule update vendor/yasnippet

# Install Zencoding
git submodule init vendor/zencoding
git submodule update vendor/zencoding

# Install RHTML
git submodule init vendor/rhtml
git submodule update vendor/rhtml

# Install textmate mode
git submodule init vendor/textmate.el
git submodule update vendor/textmate.el

# Install wrap-region
git submodule init vendor/wrap-region
git submodule update vendor/wrap-region

# Install drag-stuff
git submodule init vendor/drag-stuff
git submodule update vendor/drag-stuff

# Install mo-git-blame
git submodule init vendor/mo-git-blame
git submodule update vendor/mo-git-blame

