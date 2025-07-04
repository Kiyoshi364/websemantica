#!/bin/sh

set -xe

pl="scryer-prolog --no-add-history -f"

$pl -g 'gengraph("familytree.dot")' -g halt familytree.pl
dot -Tsvg -O familytree.dot
