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
If you're project does not follow OCaml naming conventions for variables, `--guess-var=<REGEX>` is highly reccomended with `<REGEX>` being a regular expressions that match uppercase identifiers that are actually supposed to be variables.
While being just barely better than using `sed` on the output, this dosen't convert modules.
A good starting point is `--guess-var=[A-Z]s?[0-9]?'?` (a capitial letter follwed by optional `s`, optional numeral, and optional quote `'`)