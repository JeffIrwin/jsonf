#!/bin/bash
set -e
FREEZE="$HOME/go/bin/freeze"
FONT="DejaVu Sans Mono"
BG="#1e1e2e"

{ echo '$ jsonf -s '"'"'{"a": 1 "b": 2}'"'"''; ./build/bin/jsonf -s '{"a": 1 "b": 2}' 2>&1; } \
  | "$FREEZE" --language ansi --background "$BG" --font.family "$FONT" -o img/error.png

{ echo '$ jsonf -s '"'"'{"foo": 1, "bar": 2, "baz": 3}'"'"' -p '"'"'/bark'"'"''; \
  ./build/bin/jsonf -s '{"foo": 1, "bar": 2, "baz": 3}' -p '/bark' 2>&1; } \
  | "$FREEZE" --language ansi --background "$BG" --font.family "$FONT" -o img/spell-check.png

{ echo '$ jsonf -s '"'"'[1, 2, 3,]'"'"' -Wcommas --lint'; \
  ./build/bin/jsonf -s '[1, 2, 3,]' -Wcommas --lint 2>&1; } \
  | "$FREEZE" --language ansi --background "$BG" --font.family "$FONT" -o img/warning.png

echo "All screenshots generated."
