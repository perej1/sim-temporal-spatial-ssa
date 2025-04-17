#!/bin/bash

n=$(wc -l < "sim-args.txt")
for i in $(seq 1 $n)
do
  echo "Running setting $i"
  arg=$(sed -n "${i} p" sim-args.txt)
  Rscript ${arg}
done
