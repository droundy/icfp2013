#!/bin/sh
#SBATCH --time=5
##SBATCH --mail-type ALL
#SBATCH --output parallel_submit-%j.out

time ./submit_parallel $1 $2 program master

