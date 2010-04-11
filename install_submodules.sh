#!/bin/sh
git submodule init vendor/rinari
git submodule update vendor/rinari
cd vendor/rinari
git submodule init util/jump
git submodule update util/jump
cd ../..
