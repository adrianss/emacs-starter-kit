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

