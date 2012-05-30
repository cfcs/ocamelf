open ELF_Identification
open Library


let infer_elfclass (bs: bitstring): ei_class =
  bitmatch bs with
  | { ei_class : 8 : int, offset(32), bind(read_ei_class ei_class)
    } -> ei_class

module Make
(INT : INT_t)
(B : sig val size : int end)
: sig
  module Ehdr: module type of ELF_Ehdr.Make(INT)(B)
  module Shdr: module type of ELF_Shdr.Make(INT)(B)(Ehdr)
  module Phdr: module type of ELF_Phdr.Make(INT)(B)(Ehdr)
  module Sym: module type of ELF_Sym.Make(INT)(B)(Ehdr)(Shdr)

end
= struct
  module Ehdr = ELF_Ehdr.Make(INT)(B)
  module Shdr = ELF_Shdr.Make(INT)(B)(Ehdr)
  module Phdr = ELF_Phdr.Make(INT)(B)(Ehdr)
  module Sym  = ELF_Sym.Make(INT)(B)(Ehdr)(Shdr)
end

