#!/bin/bash

set -e
set -u

stack build
bin=$(find .stack-work/install -name "plot" | xargs ls -1t | head -n 1)
timeout --foreground 1 $bin <(set -e; for i in {0..4000}; do echo "${i}   $((-30 + $RANDOM%60))"; done) || true
timeout --foreground 1 $bin <(set -e; for i in {0..4000}; do echo "$((-30 + $RANDOM%60))"; done) || true
timeout --foreground 1 $bin <(set -e; for i in {0..4000}; do echo "${i} ;  $((-30 + $RANDOM%60))"; done) || true
timeout --foreground 1 $bin <(set -e; for i in {0..4000}; do echo "${i}	$((-30 + $RANDOM%60))"; done) || true
timeout --foreground 1 $bin <(set -e; for i in {0..4000}; do echo "${i}	$((-30 + $RANDOM%60))  "; done) || true
