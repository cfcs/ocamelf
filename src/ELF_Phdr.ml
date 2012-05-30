open Library

type p_type =
      | PT_NULL
      | PT_LOAD
      | PT_DYNAMIC
      | PT_INTERP
      | PT_NOTE
      | PT_SHLIB
      | PT_PHDR
      | PT_OS    of int32
      | PT_PROC  of int32
      | PT_OTHER of int32

    let string_of_p_type = function
    | PT_NULL     -> "PT_NULL"
    | PT_LOAD     -> "PT_LOAD"
    | PT_DYNAMIC  -> "PT_DYNAMIC"
    | PT_INTERP   -> "PT_INTERP"
    | PT_NOTE     -> "PT_NOTE"
    | PT_SHLIB    -> "PT_SHLIB"
    | PT_PHDR     -> "PT_PHDR"
    | PT_OS(x)    -> Int32.format "PT_OS(0x%08x)" x
    | PT_PROC(x)  -> Int32.format "PT_PROC(0x%08x)" x
    | PT_OTHER(x) -> Int32.format "PT_OTHER(0x%08x)" x

    let (read_p_type, write_p_type) =
      let (read_p_type, write_p_type) = mk_rw
        [ (0l, PT_NULL)
        ; (1l, PT_LOAD)
        ; (2l, PT_DYNAMIC)
        ; (3l, PT_INTERP)
        ; (4l, PT_NOTE)
        ; (5l, PT_SHLIB)
        ; (6l, PT_PHDR)
        ]
      in
      (
        (fun x ->
          if 0x6000_0000l <= x && x <= 0x6FFF_FFFFl
          then PT_OS(x)
          else if 0x7000_0000l <= x && x <= 0x7FFF_FFFFl
          then PT_PROC(x)
          else
            try read_p_type x
            with Not_found -> PT_OTHER(x)
        ),
        (function
        | PT_OS(x)    -> x
        | PT_PROC(x)  -> x
        | PT_OTHER(x) -> x
        | x           -> write_p_type x
        )
      )


module Make
(INT : INT_t)
(B : sig val size : int end)
(Ehdr : module type of ELF_Ehdr.Make(INT)(B))
: sig
    type elf_phdr =
        { p_type   : p_type
        ; p_flags  : bitstring
        ; p_offset : INT.t
        ; p_vaddr  : INT.t
        ; p_paddr  : INT.t
        ; p_filesz : INT.t
        ; p_memsz  : INT.t
        ; p_align  : INT.t
        }
  val read : Ehdr.elf_ehdr -> Bitstring.bitstring -> elf_phdr array
end
= struct
    type elf_phdr =
        { p_type   : p_type
        ; p_flags  : bitstring
        ; p_offset : INT.t
        ; p_vaddr  : INT.t
        ; p_paddr  : INT.t
        ; p_filesz : INT.t
        ; p_memsz  : INT.t
        ; p_align  : INT.t
        }

    module Ehdr = ELF_Ehdr.Make(INT)(B)
    module SafeX = Safe.Make(INT)(B)

    let read (ehdr: Ehdr.elf_ehdr) (bs: bitstring) =
      let endian = ehdr.endian in
        let read_nth (n: int) =
        let sz = n * ehdr.e_phentsize in
        let phdr_bit_ofs = SafeX.(
          (of_int 8) * (ehdr.e_phoff + (of_int sz))
        |> to_int ) in
        bitmatch Bitstring.dropbits phdr_bit_ofs bs with
          { p_type   : 32 : endian(endian), bind(read_p_type p_type)
          ; p_flags  : 32 : bitstring
          ; p_offset : B.size : bitstring, bind(SafeX.(read_word p_offset)|>fst)
          ; p_vaddr  : B.size : bitstring, bind(SafeX.(read_word p_vaddr)|>fst)
          ; p_paddr  : B.size : bitstring, bind(SafeX.(read_word p_paddr)|>fst)
          ; p_filesz : B.size : bitstring, bind(SafeX.(read_word p_filesz)|>fst)
          ; p_memsz  : B.size : bitstring, bind(SafeX.(read_word p_memsz)|>fst)
          ; p_align  : B.size : bitstring, bind(SafeX.(read_word p_align)|>fst)
          } ->
            { p_type
            ; p_offset
            ; p_vaddr
            ; p_paddr
            ; p_filesz
            ; p_memsz
            ; p_flags
            ; p_align
            }
      in
      Array.init ehdr.e_phnum read_nth

    let to_string ph =
      Printf.sprintf
        "
{ p_type   = %s
; p_flags  = %s
; p_offset = %s
; p_vaddr  = %s
; p_paddr  = %s
; p_filesz = %s
; p_memsz  = %s
; p_align  = %s
}"
        (string_of_p_type    ph.p_type  )
        (string_of_bitstring ph.p_flags )
        (SafeX.to_string_x   ph.p_offset)
        (SafeX.to_string_x   ph.p_vaddr )
        (SafeX.to_string_x   ph.p_paddr )
        (SafeX.to_string_x   ph.p_filesz)
        (SafeX.to_string_x   ph.p_memsz )
        (SafeX.to_string_x   ph.p_align )

end
