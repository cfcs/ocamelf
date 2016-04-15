OCamELF
=======

ELF-parsing library written in OCaml for OCaml

NOTE: This repository is a mutilation of the original
=====================================================

It's pretty messy, but it rewrites ELF files in the following way:
- `ELF_Shdr.Make.strtab_entry_from_offset`: Adds a `DT_NEEDED` entry in all `SHT_DYNAMIC` sections (usually there is only one: `.dynsym`). The entry will mirror the first `DT_NEEDED` entry it sees to point to the same string, but with an added offset of two bytes (`libc.so.6` becomes `bc.so.6`, and so on). Although I couldn't find this documented in the spec (I probably missed it), the Linux loader didn't actually load the library unless if the entry was placed after the first non-DT_NEEDED-entry, so we shuffle the `INIT` entry around a bit.

- `ELF_Sym.Make.make_weak_bind`: Changes the binding type of all symbols to weak. This causes them to be overridden by external libraries defining the same symbols. You may want to filter that list a bit, such as only doing it for `sym.st_value = INT.zero` or when `sym.st_type = STT_FUNC && sym.st_bind = STT_GLOBAL`.

So this is basically now a ghetto tool for static hooking.

Strong requirements
-------------------

* OCaml >= 3.12
> http://caml.inria.fr/

* bistring >= 2.0.3
> http://code.google.com/p/bitstring/

Possibly optional requirements
------------------------------

If you want to build the examples using the Makefile, or want to build
anything using a similar compilation scheme, you will need ocamlbuild.
How to properly install and set this up is left as an exercise for the reader!

Specifics of getting a bitstring-enabled program to compile can be found on
this page:

http://people.redhat.com/~rjones/bitstring/html/Bitstring.html#2_Compiling

Details are sordid, deal with it!
