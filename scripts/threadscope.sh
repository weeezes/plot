#!/bin/bash

stack build --ghc-options="-eventlog -threaded -rtsopts"
bin=$(find .stack-work/install -name "plot" | xargs ls -1t | head -n 1)
timeout --foreground 60 $bin -k -- <(set -e; for i in {0..100000}; do echo "${i}   $(($RANDOM%60))"; done) +RTS -ls -N2