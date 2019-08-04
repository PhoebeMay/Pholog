#!/bin/bash

major=$1
minor=$2
testcase=$3


log="$HOME/Dropbox/Stuff1819/proooject/Project/code/src/scripts/gc_log"


echo $minor >> $log 
echo $major >> $log 

cd /home/phoebe/Dropbox/Stuff1819/proooject/Project/code/src


OCAMLRUNPARAM="=$major,s=$minor"
export OCAMLRUNPARAM="h=$major,s=$minor"


echo $OCAMLRUNPARAM >> $log

OCAMLRUNPARAM="h=$major,s=$minor" dune build ./runFromFile.exe
# OCAMLRUNPARAM="h=$major,s=$minor" /usr/bin/time -f '%U' ./_build/default/runFromFile.exe "$testcase-pho"
OCAMLRUNPARAM="h=$major,s=$minor" /usr/bin/time -f '%U' dune exec ./runFromFile.exe "$testcase-pho"

