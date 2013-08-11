#!/bin/sh

set -ev

for x in check_status test_simple get_training lookup_problems make_guess submit_parallel; do
   ghc --make -O2 $x.hs
done
