#!/bin/sh

set -xe

Prefix='"test_"'

function run_test {
  time scryer-prolog --no-add-history -f -g "run_tests(""$1"", ""$Prefix"")" -g halt test_implementation.pl
}

run_test "semweb_unord_lists"
