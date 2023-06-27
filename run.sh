#!/bin/bash



set -e 
file_name=$1

ocamlopt utils.ml $file_name   -o main
./main
rm main
rm *.cmi *.cmx *.o