# VeriDum

## Compilation

Install all dependencies with:

```sh
opam install . --deps-only
```

then compile with:

```sh
dune build
```

## Usage

Run with:

```sh
_build/default/bin/main.exe [--sl|--isl] [--step-exec] [--no-verbose] [--keep-dummies] <file>
```

Whether to use SL+ or ISL+ is determined by the file extension (`.sl` and `.isl` respectively) and can be overridden with the `--sl` and `--isl` flags.

The `--step-exec` flag enables interactive stepping (enter `n` to execute the next command, `c` to run until the end) and picking of the branch/number of iterations in ISL+.

The final postconditions are pruned of unused dummy variables unless `--keep-dummies` is provided.
