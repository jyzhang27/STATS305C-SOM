#!/bin/bash

projectdir=$(dirname "BASH_SOURCE")
cd $projectdir

mkdir -p deps

# clone data repo
if [ ! -d "deps/transfers" ]; then
    cd deps
    git clone https://github.com/ewenme/transfers.git
    cd ..
fi
