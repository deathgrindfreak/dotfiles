#!/usr/bin/env bash

boop() (
  local last="$?";

  if [ $last -eq 0 ]; then
    sfx good
  else
    sfx bad
  fi

  $(exit "$last")
)
