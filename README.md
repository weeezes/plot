# test
stack build --profile --executable-profiling
stack exec -- test-exe +RTS -p
stack build && stack exec test-exe
stack build --ghc-options="-eventlog -threaded -rtsopts"
stack exec -- test-exe +RTS -ls -N2
