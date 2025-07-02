#!/bin/sh

set -xe

Goal='run_tests'

function run_test {
  time scryer-prolog --no-add-history -f -g "$Goal" -g halt test_"$1".pl
}

run_test "ttlcat"
run_test "ttl2pl"
