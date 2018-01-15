#!/bin/bash

set -e
set -u

stack build
stack exec -- test-exe <(set -e; for i in {0..4000}; do echo $(($RANDOM%60)); done)