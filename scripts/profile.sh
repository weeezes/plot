#!/bin/bash

stack build --profile --executable-profiling
bin=$(find .stack-work/install -name "plot" | xargs ls -1t | head -n 1)
$bin <(set -e; for i in {0..4000}; do echo "${i}   $(($RANDOM%60))"; done) +RTS -p
