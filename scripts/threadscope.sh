#!/bin/bash

stack build --ghc-options="-eventlog -threaded -rtsopts"
bin=$(find .stack-work/install -name "plot" | xargs ls -1t | head -n 1)
timeout --foreground 60 $bin +RTS -ls -N2