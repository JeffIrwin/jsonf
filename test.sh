#!/usr/bin/env bash

set -exu

flags=()
flags+=("-Wno-tabs")

fpm install --prefix . --flag "${flags[@]}" && ./bin/jsonf -s '{"a123": 69, "x456": 420}'

#fpm run -- data/in1.json
#fpm test

