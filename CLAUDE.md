# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

```bash
fpm build                        # build
fpm test                         # run all tests
fpm test --flag "-Wno-tabs"      # run tests suppressing tab warnings (used in test.sh)
fpm run -- FILE.json             # run on a json file
fpm run -- -s '{\"a\": 1}'      # run on a string (quotes must be escaped inside fpm)
fpm install --prefix build --profile debug  # install to ./build/bin/jsonf for direct invocation
./build/bin/jsonf -s '{"a": 1}'            # run the installed binary directly
```

Note: When passing JSON strings via `fpm run --`, quotes inside the string must be escaped as `\"`. When running `./bin/jsonf` directly, normal quoting works.

CI runs via Docker (`docker build . -t rocky`), which tests debug, release, and various CLI flags.

## Architecture

This is a Fortran JSON parser and formatter implemented as a library + CLI using [fpm](https://fpm.fortran-lang.org/).

### Module layout

- `src/utils.F90` — `jsonf__utils`: string utilities, dynamic vectors (`str_builder_t`, `str_vec_t`, `i64_vec_t`), character helpers, ANSI color constants, `panic()`, `jsonf_exit()`
- `src/blarg.F90` — `jsonf__blarg`: linear algebra / array utility routines (ported from another project; used for `range_i32`, `levenshtein`)
- `src/sort.F90` — `jsonf__sort`: sorting routines
- `src/jsonf.F90` — `jsonf`: the core module. Contains all types and parsing/printing logic
- `app/args.F90` — `jsonf__args`: CLI argument parsing (`args_t`, `parse_args()`)
- `app/main.F90` — `jsonf__app` + `program main`: wires args to JSON operations
- `test/test.F90` — `jsonf__test` + `program test`: all unit tests

### Key types (in `src/jsonf.F90`)

- `stream_t` — character-by-character source (file or string)
- `lexer_t` — tokenizer; tracks line/col for error reporting; holds `stream_t`
- `token_t` — a single token with kind, line, col, text, and scalar value
- `sca_t` — scalar value (bool, i64, f64, str, or null)
- `json_val_t` — a JSON value node (scalar, array, or object); objects use an open-addressing hashmap with `djb2_hash` and linear probing; insertion order is preserved via an `idx(:)` array
- `json_t` — top-level type; holds configuration options, the root `json_val_t`, and accumulated diagnostics

### Parsing pipeline

`json%read_file()` / `json%read_str()` → `parse_json()` → `new_lexer()` + `parse_val()` (recursive descent: `parse_obj()`, `parse_arr()`) → stores result in `json%root`

Error messages are Rust-style with source location and `^`-underlines. Errors are accumulated in `lexer%diagnostics` (a `str_vec_t`) and propagated to `json%diagnostics` after parsing. The `json%print_errors_immediately` flag controls whether they are also printed inline.

### Token kinds

Integer constants, defined at the top of `src/jsonf.F90`. `kind_name()` maps them to strings for debugging.

### Fortran-specific notes

- Uses `kind=8` for i64/f64 throughout
- Uses C preprocessor macros (`#define TEST(...)`) in test code; source files use `.F90` extension (uppercase) to enable preprocessing
- `implicit none` is enforced globally via `fpm.toml` (`[fortran] implicit-typing = false`)
- Fortran string comparison with `==` ignores trailing spaces; always use `is_str_eq()` from `utils` for correct comparison
- `move_val()` uses `move_alloc` for performance; `copy_val()` is recursive and should be avoided when possible
