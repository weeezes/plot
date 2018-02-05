#!/bin/bash

stack build --profile --executable-profiling
bin=$(find .stack-work/install -name "plot" | xargs ls -1t | head -n 1)
time $bin -k -s +RTS -p -- <(set -e; for i in {0..1000000}; do echo "${i}   $(($RANDOM%60))"; done)
