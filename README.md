# test
stack build --profile --executable-profiling
stack exec -- test-exe +RTS -p
stack build && stack exec test-exe
