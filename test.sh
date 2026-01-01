#!/usr/bin/env bash

set -exu

flags=()
flags+=("-Wno-tabs")

fpm test --flag "${flags[@]}"
fpm run  --flag "${flags[@]}" -- data/in1.json
fpm install --prefix . --flag "${flags[@]}"

./bin/jsonf -s '{"a123": 69, "x456": 420}'

