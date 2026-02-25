
# jsonf

A read-only json parser in Fortran I guess

## Why another Fortran json parser?

There's no good reason -- for an alternative I recommend [json-fortran](https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage)

## Build

Use [fpm](https://fpm.fortran-lang.org/), the fortran package manager, with commands such as these:
```bash
fpm build
fpm install
```

With `fpm install`, the `jsonf` binary is installed to the default fpm path,
usually `~/.local/bin`.

## Run

Use fpm as a wrapper or run the binary directly.  With fpm:
```bash
fpm run -- --help
```

If the binary is in your path, you can simply run `jsonf --help`.  Here are some
of the current options
```
jsonf 0.3.0
 Usage:
     jsonf -h | --help
     jsonf FILE.json [(-p|--pointer) POINTER]
     jsonf (-s|--string) STRING [(-p|--pointer) POINTER]
     jsonf -c|--compact
     jsonf -d|--no-dup
     jsonf -f|--first-dup
     jsonf -l|--lint
     jsonf -q|--quiet
     jsonf -t|--tokens
     jsonf --version

 Options:
     --help       Show this help
     FILE.json    Input json filename
     --string     Input json string
     --pointer    json pointer path
     --lint       Check json for syntax errors
     --compact    Format compactly without whitespace
     --first-dup  Keep first duplicate key, default last
     --no-dup     Do not allow duplicate keys
     --quiet      Decrease log verbosity
     --tokens     Dump tokens without parsing json
     --version    Show the jsonf version number
```

## Features

The json can be loaded from a file or a string:
```bash
jsonf data/in8.json
jsonf --string '{"a": 1, "b": 2}'
jsonf -s '{"a": 1, "b": 2}'  # short form of --string
```

Beware that FPM has extra quote escape rules for command arguments:
```bash
fpm run -- -s '{\"a\": 1, \"b\": 2}'
```

### Get value by path

Use standard [json pointer syntax](https://datatracker.ietf.org/doc/html/rfc6901)
with the `--pointer` or `-p` argument to get a value by its path:
```bash
jsonf --string '{"a": 1, "b": 2}' --pointer '/a'
# 1

jsonf -s '{"foo": 3, "bar": {"a": 1, "b": 2}}' -p '/bar/b'
# 2

jsonf -s '{"foo": 3, "baz": [0, 10, 20, 30]}' -p '/baz/3'
# 30
```

### Spell check

If a key doesn't exist, the closest match is suggested:
```bash
jsonf -s '{"foo": 1, "bar": 2, "baz": 3}' -p '/bark'
# Error: key "bark" not found
# Did you mean "bar"?
```

### Formatting and linting

By default, the whole json is echoed and formatted if no pointer is provided

Linting can also be performed without echoing by using the `--lint` or `-l`
argument.  If the json is correct, nothing is printed and a 0 exit code is
returned

<!--
TODO
## API

All of the command-line options can also be used in your Fortran programs using
jsonf as a library

Error handling needs work before I can recommend API usage.  Any json syntax
errors will stop the program

Documentation TBD
-->
