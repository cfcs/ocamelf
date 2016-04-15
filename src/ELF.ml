open ELF_Identification
open Library

module Bit32 : INT_t = struct include Int32
  let wordsize = 32
  let of_int32 i = i
  let to_int32 = of_int32
  let to_int64 = Int64.of_int32
  let of_int64 = Int64.to_int32
end
module Bit64 : INT_t = struct include Int64
  let wordsize = 64
  let to_int64 i = i
  let of_int64 = to_int64
end

let infer_elfclass (bs: bitstring): ei_class =
  bitmatch bs with
  | { ei_class : 8 : int, offset(32), bind(read_ei_class ei_class)
    } -> ei_class

let infer_elf_int_type (bs : bitstring) : (module INT_t) =
  match infer_elfclass bs with
  | ELFCLASS32 -> (module Bit32 : INT_t)
  | ELFCLASS64 -> (module Bit64 : INT_t)
  | ELFCLASSNONE -> failwith "Unsupported ELF class"
  
module type Make_t =
functor(INT : INT_t )
-> sig
  module INT : INT_t
  module Ehdr: module type of ELF_Ehdr.Make(INT)
  module Shdr: module type of ELF_Shdr.Make(INT)
  module Shdr_DYNAMIC: module type of ELF_Shdr.Elf_Shdr_DYNAMIC(INT)
  module Phdr: module type of ELF_Phdr.Make(INT)(Ehdr)
  module Sym: module type of ELF_Sym.Make(INT)(Ehdr)(Shdr)
end

module Make : Make_t =
functor (INT : INT_t) ->
struct
  module INT = INT
  module Ehdr = ELF_Ehdr.Make(INT)
  module Shdr = ELF_Shdr.Make(INT)
  module Shdr_DYNAMIC = ELF_Shdr.Elf_Shdr_DYNAMIC(INT)
  module Phdr = ELF_Phdr.Make(INT)(Ehdr)
  module Sym  = ELF_Sym.Make(INT)(Ehdr)(Shdr)
end

