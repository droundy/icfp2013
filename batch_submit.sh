#!/bin/sh
#SBATCH
##SBATCH --mail-type ALL
#SBATCH --output paralell-$1-$2

time ./submit_parallel $1 $2 train master

