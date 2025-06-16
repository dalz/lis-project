# VeriDum

A tool for computing postconditions in Separation Logic (SL+) and Incorrectness Separation Logic (ISL+) through symbolic execution.

The user can give a SL+ or ISL+ program as input, together with a precondition and get a postcondition as output. 

For detailed theoretical background and implementation details, see the [report](report/report.tex). The section "Front-end" provides additional details on the usage of the tool.


## Compilation

Prerequisites:
- OCaml (>= 5.1.0)
- Dune (>= 3.18)
- OPAM package manager

Install all dependencies with:

```sh
opam install . --deps-only
```

Then compile with:

```sh
dune build
```

Optionally, you can install the tool globally:

```sh
dune install
```
This allows you to use the command `veridum` directly from anywhere in your terminal, without needing to specify the path to the executable.


## Usage

### Basic Command Structure

```sh
veridum [--sl|--isl] [--step-exec] [--no-verbose] [--keep-dummies] <file>
```

Where `<file>` is the path to your SL+ or ISL+ program file, as shown in some examples contained in the `examples/` directory.

**Command Options:**
- `--help`: Show usage information
- `--no-verbose`: Reduce output verbosity
- `--keep-dummies`: Keep dummy variables in the output postcondition (otherwise, unused dummy variables are pruned)
- `--sl` / `--isl`: Force SL+ or ISL+ mode
- `--step-exec`: Enable interactive stepping through execution:
  - Press `n` to execute the next command
  - Press `c` to run until completion
  - Choose branches and iteration counts (in ISL+ programs)

**Note:** If you installed the project using `dune install`, you can use `veridum` directly. Otherwise, replace `veridum` with the path to your executable (typically `_build/default/bin/main.exe`).

### Input Methods

If you want to compute postconditions, you need to provide a SL+ or ISL+ program along with a precondition. There are two ways to do this:

#### 1. File Input
Provide a file containing your SL+ or ISL+ program. The logic type is automatically detected from the file extension (`.sl` for SL+, `.isl` for ISL+), but you can override it with `--sl` or `--isl`.

```bash
# Loads a SL+ program from file and computes postcondition
veridum examples/my_example.sl

# Loads an ISL+ program from file and computes postcondition
veridum examples/my_example.isl
```

Some example files are provided in the `examples/` directory.

#### 2. Terminal Input
If you do not want to use a file, you can choose to type your program directly into the terminal via standard input.
You need to specify the logic type with `--sl` or `--isl`.

```bash
# Starts an interactive session where you can type your SL+ program
veridum --sl

# Starts an interactive session where you can type your ISL+ program
veridum --isl
```

