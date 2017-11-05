# MACO: Virtual Machine in Ocaml

MACO is a simple Ocaml port of the [MAC](https://github.com/felixangell/mac) project.

## How to use:
You should use the project with the latest version of the ocaml compiler.
You can optain the latest version of ocaml with the use of the [opam switch command](https://opam.ocaml.org/doc/Usage.html#opam-switch).

You only need jbuilder for this project:
```sh
opam install jbuilder
```

Clone:
```sh
git clone https://github.com/StrykerKKD/maco.git
cd vmao
```

Build:
```sh
jbuilder build maco.exe
```

Run:
```sh
./_build/default/maco.exe
```

## Project structure:
* maco.ml: Simple imperative virtual machine
* macofp.ml: Simple functional virtual machine