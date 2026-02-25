#!/usr/bin/env bash

set -exu

flags=()
flags+=("-Wno-tabs")

fpm test --flag "${flags[@]}"
fpm run  --flag "${flags[@]}" -- data/in1.json
#fpm run  --flag "${flags[@]}" -- data/in2.json
fpm run  --flag "${flags[@]}" -- data/in3.json
fpm install --prefix build --flag "${flags[@]}" --profile debug

./build/bin/jsonf -s '{"a123": 69, "x456": 420}'

./build/bin/jsonf -s '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}'

