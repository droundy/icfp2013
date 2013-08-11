#!/bin/bash

shopt -s nullglob
for id in `ls problems/$1/`
do
    sbatch "-c4 batch_submit.sh $1 $id"
done