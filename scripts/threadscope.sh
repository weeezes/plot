#!/bin/bash

stack build --ghc-options="-eventlog -threaded -rtsopts"
stack exec -- test-exe +RTS -ls -N2