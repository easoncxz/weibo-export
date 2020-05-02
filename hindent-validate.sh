#!/usr/bin/env bash

# Yeah let's ask for a global install
( which hindent > /dev/null ) || stack install indent

if which parallel > /dev/null
then
  alias xargs=parallel
fi

# https://stackoverflow.com/questions/11003418/calling-shell-functions-with-xargs
check() {
  filepath="$1"
  hindent --validate "$filepath"
}
export -f check

find "src" "test" "app" -name '*.hs' -print0 | xargs -0 -I {} bash -c 'check "{}"'
