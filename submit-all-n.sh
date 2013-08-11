#!/bin/bash

for id in `ls problems/$1/`
do
    sbatch "-c8" "batch_submit.sh" "$1" "$id"
done