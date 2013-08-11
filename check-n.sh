#!/bin/bash

shopt -s nullglob
echo "Total of size $1: `ls problems/$1 | wc -l`, Number solved: `ls problems/$1/*/solved | wc -l`"