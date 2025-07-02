#!/bin/sh

set -xe

pushd reference_implementations
./run_tests.sh
popd

pushd parsers
./run_tests_turtle.sh
popd

pushd utils
./run_tests.sh
popd
