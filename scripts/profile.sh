#!/bin/bash

stack build --profile --executable-profiling
stack exec -- test-exe <(set -e; for i in {0..4000}; do echo "${i}   $(($RANDOM%60))"; done) +RTS -p 
