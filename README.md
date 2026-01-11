# Shibboleth, a converter from SML to OCaml 

This is a converter for OCaml written primarly in OCaml using the Dune package managmer.

## Usage 

### Installing 
The install of this should be relativly simple.
If you have all the prerequisites, just clone this repo into a local directory and run `make install` inside that directory.
After that, the command `shibboleth` will run the tool!

### Usage 

> [!INFO]
> It is *highly* reccomended that you transform files of the form `A.sig`, `A.fun`, and `A.sml` into one single `A.ml`, because of the way OCaml's module system works. This can be done by specifying `--concat-output`, as in `shibboleth file --concat-output A.sig A.fun A.sml`

By default, this tool outputs to standard output. 
To instead output to a file, use the `-o` or `--output` option 

### Limatitations 

Currently, this tool has a number of shortcomings:
1. Name capitilzation (ie, `datatype Ctx`) is not fixed (`type Ctx`) in the output 
2. Because of this, `ocamlformat` can't, by default format the output (unless your code is already correctly capitlized)

## Requirements 

The tools required to build this are as follows:
- `dune`, `opam`, `ocaml`, for the OCaml side 
- GNU `make`, for the scripts 

The OCaml libraries required to build this are as follows:
- `alcotest` (testing)
- `ppxlib` (core OCaml AST)
- `cmdliner` (command line args)
- `menhir` (parsing)
- `ppx_import` (utility) 
- `ppx_deriving` (utility)
- `ez_file` (utility)
- `ocamlformat` (formatting)
- `odoc` (documentation)
- `re` (regex) 
