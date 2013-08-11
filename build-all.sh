#!/bin/sh

set -ev

for x in check_status test_simple get_training lookup_problems make_guess make_guess_bonus make_guess_simple submit_parallel; do
   ghc --make -threaded -O2 $x.hs
done
