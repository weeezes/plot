#!/bin/bash

stack build --profile --executable-profiling
stack exec -- test-exe +RTS -p