#!/usr/bin/env sh

set -eou pipefail

script_name="$1"; shift

stack script --resolver lts-20.11 \
      --package turtle \
      --package process \
      "./src/$script_name" -- "$@"
