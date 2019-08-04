#!/bin/bash

tc=$1

cd ~/Dropbox/Stuff1819/proooject/Project/code/src

dune exec ./writeToFile.exe "demo.pl" $tc

dune build ./runFromFile.exe

./_build/default/runFromFile.exe "demo.pl"
