#!/usr/bin/env bash

set -exu

flags=()
flags+=("-Wno-tabs")

fpm test --flag "${flags[@]}"
fpm run  --flag "${flags[@]}" -- data/in1.json
#fpm run  --flag "${flags[@]}" -- data/in2.json
fpm run  --flag "${flags[@]}" -- data/in3.json
fpm install --prefix . --flag "${flags[@]}" --profile debug

./bin/jsonf -s '{"a123": 69, "x456": 420}'

./bin/jsonf -s '{"a123": 69, "nested": {"p": 1337, "q": 9999}, "x456": 420}'

