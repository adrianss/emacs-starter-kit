#!/bin/sh
git submodule init vendor/rinari
git submodule update vendor/rinari
cd vendor/rinari
git submodule init utils/jump
git submodule update utils/jump
cd ../..
