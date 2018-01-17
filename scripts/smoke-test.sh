#!/bin/bash

set -e
set -u

stack build
timeout --foreground 12 stack exec -- test-exe <(set -e; for i in {0..4000}; do echo "${i}   $(($RANDOM%60))"; done) || true
timeout --foreground 12 stack exec -- test-exe -s -- <(set -e; for i in {0..4000}; do echo "$(($RANDOM%60))"; done) || true
