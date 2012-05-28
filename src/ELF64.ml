open ELF
open Library

type ei_osabi =
  | ELFOSABI_SYSV
  | ELFOSABI_HPUX
  | ELFOSABI_STANDALONE

let (read_ei_osabi, write_ei_osabi) = mk_rw
  [ (  0, ELFOSABI_SYSV)
  ; (  1, ELFOSABI_HPUX)
  ; (255, ELFOSABI_STANDALONE)
  ]

let string_of_ei_osabi = function
| ELFOSABI_SYSV       -> "ELFOSABI_SYSV"
| ELFOSABI_HPUX       -> "ELFOSABI_HPUX"
| ELFOSABI_STANDALONE -> "ELFOSABI_STANDALONE"

type ei_abiversion = int

module Make : ELFCLASS =
struct
  module Identification =
  struct 
    open ELF_Identification

    type elf_identification =
        { ei_class      : ei_class
        ; ei_data       : ei_data
        ; ei_version    : ei_version
        ; ei_osabi      : ei_osabi
        ; ei_abiversion : ei_abiversion
        }

    let read (bs: bitstring): elf_identification =
      bitmatch bs with
      | { 0x7F          : 8
        ; "ELF"         : 24 : string
        ; ei_class      : 8  : int, bind (read_ei_class   ei_class)
        ; ei_data       : 8  : int, bind (read_ei_data    ei_data)
        ; ei_version    : 8  : int, bind (read_ei_version ei_version)
        ; ei_osabi      : 8  : int, bind (read_ei_osabi   ei_osabi)
        ; ei_abiversion : 8  : int
        ; padding       : 56 : bitstring
        } ->
          assert (is_zeros padding 56);
          { ei_class
          ; ei_data
          ; ei_version
          ; ei_osabi
          ; ei_abiversion
          }

    let write (ei: elf_identification): bitstring =
      BITSTRING
        { 0x7F                           : 8
        ; "ELF"                          : 24 : string
        ; write_ei_class   ei.ei_class   : 8
        ; write_ei_data    ei.ei_data    : 8
        ; write_ei_version ei.ei_version : 8
        ; write_ei_osabi   ei.ei_osabi   : 8
        ; ei.ei_abiversion               : 8
        ; Bitstring.create_bitstring 56  : 56 : bitstring
        }

    let to_string (ei: elf_identification): string =
      Printf.sprintf
        "
{ ei_class      = %s
; ei_data       = %s
; ei_version    = %s
; ei_osabi      = %s
; ei_abiversion = %s
}"
        (string_of_ei_class   ei.ei_class)
        (string_of_ei_data    ei.ei_data)
        (string_of_ei_version ei.ei_version)
        (string_of_ei_osabi   ei.ei_osabi)
        (string_of_int        ei.ei_abiversion)
  end

  module Ehdr =
  struct
    open ELF_Ehdr

    type et =
      | ET_NONE
      | ET_REL
      | ET_EXEC
      | ET_DYN
      | ET_CORE
      | ET_OS    of int
      | ET_PROC  of int
      | ET_OTHER of int

    let (read_et, write_et) =
      let (read_et, write_et) = mk_rw
        [ (0, ET_NONE)
        ; (1, ET_REL)
        ; (2, ET_EXEC)
        ; (3, ET_DYN)
        ; (4, ET_CORE)
        ]
      in
      (
        (fun x ->
          if 0xFE00 <= x && x <= 0xFEFF
          then ET_OS(x)
          else if 0xFF00 <= x && x <= 0xFFFF
          then ET_PROC(x)
          else
            try read_et x
            with Not_found -> ET_OTHER(x)
        ),
        (function
        | ET_OS(x)    -> x
        | ET_PROC(x)  -> x
        | ET_OTHER(x) -> x
        | x           -> write_et x
        )
      )

    let string_of_et = function
    | ET_NONE     -> "ET_NONE"
    | ET_REL      -> "ET_REL"
    | ET_EXEC     -> "ET_EXEC"
    | ET_DYN      -> "ET_DYN"
    | ET_CORE     -> "ET_CORE"
    | ET_OS(x)    -> "ET_OS("    ^ string_of_int x ^ ")"
    | ET_PROC(x)  -> "ET_PROC("  ^ string_of_int x ^ ")"
    | ET_OTHER(x) -> "ET_OTHER(" ^ string_of_int x ^ ")"

    open Identification

    type elf_ehdr =
        { e_ident     : elf_identification
        ; e_type      : et
        ; e_machine   : em
        ; e_version   : ev
        ; e_entry     : int64
        ; e_phoff     : int64
        ; e_shoff     : int64
        ; e_flags     : bitstring
        ; e_ehsize    : int
        ; e_phentsize : int
        ; e_phnum     : int
        ; e_shentsize : int
        ; e_shnum     : int
        ; e_shstrndx  : int
        (* extra: *)
        ; endian      : Bitstring.endian
        }

    open ELF_Identification

    let read (e_ident: elf_identification) (bs: bitstring): elf_ehdr =
      let endian = endian_of_ei_data e_ident.ei_data in
      bitmatch Bitstring.dropbits 128 bs with
        { e_type      : 16 : int, endian(endian), bind (read_et e_type)
        ; e_machine   : 16 : int, endian(endian), bind (read_em e_machine)
        ; e_version   : 32 : int, endian(endian), bind (read_ev e_version)
        ; e_entry     : 64 : int, endian(endian)
        ; e_phoff     : 64 : int, endian(endian)
        ; e_shoff     : 64 : int, endian(endian)
        ; e_flags     : 32 : bitstring
        ; e_ehsize    : 16 : int, endian(endian)
        ; e_phentsize : 16 : int, endian(endian)
        ; e_phnum     : 16 : int, endian(endian)
        ; e_shentsize : 16 : int, endian(endian)
        ; e_shnum     : 16 : int, endian(endian)
        ; e_shstrndx  : 16 : int, endian(endian)
        } ->
          { e_ident
          ; e_type
          ; e_machine
          ; e_version
          ; e_entry
          ; e_phoff
          ; e_shoff
          ; e_flags
          ; e_ehsize
          ; e_phentsize
          ; e_phnum
          ; e_shentsize
          ; e_shnum
          ; e_shstrndx
          ; endian
          }

    let to_string eh =
      Printf.sprintf
        "
{ e_ident     = %s
; e_type      = %s
; e_machine   = %s
; e_version   = %s
; e_entry     = %s
; e_phoff     = %s
; e_shoff     = %s
; e_flags     = %s
; e_ehsize    = %s
; e_phentsize = %s
; e_phnum     = %s
; e_shentsize = %s
; e_shnum     = %s
; e_shstrndx  = %s
}"
        (Identification.to_string eh.e_ident)
        (string_of_et             eh.e_type)
        (string_of_em             eh.e_machine)
        (string_of_ev             eh.e_version)
        (string_of_int64_x        eh.e_entry)
        (string_of_int64_x        eh.e_phoff)
        (string_of_int64_x        eh.e_shoff)
        (string_of_bitstring      eh.e_flags)
        (string_of_int            eh.e_ehsize)
        (string_of_int            eh.e_phentsize)
        (string_of_int            eh.e_phnum)
        (string_of_int            eh.e_shentsize)
        (string_of_int            eh.e_shnum)
        (string_of_int            eh.e_shstrndx)

  end

  module Shdr =
  struct

    type sh_type =
      | SHT_NULL
      | SHT_PROGBITS
      | SHT_SYMTAB
      | SHT_STRTAB
      | SHT_RELA
      | SHT_HASH
      | SHT_DYNAMIC
      | SHT_NOTE
      | SHT_NOBITS
      | SHT_REL
      | SHT_SHLIB
      | SHT_DYNSYM
      | SHT_OS    of int32
      | SHT_PROC  of int32
      | SHT_OTHER of int32

    let (read_sh_type, write_sh_type) =
      let (read_sh_type, write_sh_type) = mk_rw
        [ ( 0l, SHT_NULL    )
        ; ( 1l, SHT_PROGBITS)
        ; ( 2l, SHT_SYMTAB  )
        ; ( 3l, SHT_STRTAB  )
        ; ( 4l, SHT_RELA    )
        ; ( 5l, SHT_HASH    )
        ; ( 6l, SHT_DYNAMIC )
        ; ( 7l, SHT_NOTE    )
        ; ( 8l, SHT_NOBITS  )
        ; ( 9l, SHT_REL     )
        ; (10l, SHT_SHLIB   )
        ; (11l, SHT_DYNSYM  )
        ]
      in
      (
        (fun x ->
          if 0x6000_0000l <= x && x <= 0x6FFF_FFFFl
          then SHT_OS(x)
          else if 0x7000_0000l <= x && x <= 0x7FFF_FFFFl
          then SHT_PROC(x)
          else
            try read_sh_type x
            with Not_found -> SHT_OTHER(x)
        ),
        (function
        | SHT_OS(x)    -> x
        | SHT_PROC(x)  -> x
        | SHT_OTHER(x) -> x
        | x            -> write_sh_type x
        )
      )

    let string_of_sh_type = function
    | SHT_NULL     -> "SHT_NULL"
    | SHT_PROGBITS -> "SHT_PROGBITS"
    | SHT_SYMTAB   -> "SHT_SYMTAB"
    | SHT_STRTAB   -> "SHT_STRTAB"
    | SHT_RELA     -> "SHT_RELA"
    | SHT_HASH     -> "SHT_HASH"
    | SHT_DYNAMIC  -> "SHT_DYNAMIC"
    | SHT_NOTE     -> "SHT_NOTE"
    | SHT_NOBITS   -> "SHT_NOBITS"
    | SHT_REL      -> "SHT_REL"
    | SHT_SHLIB    -> "SHT_SHLIB"
    | SHT_DYNSYM   -> "SHT_DYNSYM"
    | SHT_OS(x)    -> "SHT_OS("    ^ string_of_int32_x x ^ ")"
    | SHT_PROC(x)  -> "SHT_PROC("  ^ string_of_int32_x x ^ ")"
    | SHT_OTHER(x) -> "SHT_OTHER(" ^ string_of_int32_x x ^ ")"

    type elf_shdr =
        { sh_name      : int32
        ; sh_type      : sh_type
        ; sh_flags     : bitstring
        ; sh_addr      : int64
        ; sh_offset    : int64
        ; sh_size      : int64
        ; sh_link      : int32
        ; sh_info      : int32
        ; sh_addralign : int64
        ; sh_entsize   : int64
        (* extra: *)
        ; name         : string
        }

    open Ehdr

    let read (e_hdr: elf_ehdr) (bs: bitstring) =
      let endian = e_hdr.endian in
      let read_nth (n: int) =
        let shdr_bit_ofs = Safe.(
          8 * (of_int64 e_hdr.e_shoff + (n * e_hdr.e_shentsize))
        ) in
        bitmatch Bitstring.dropbits shdr_bit_ofs bs with
          { sh_name      : 32 : endian(endian)
          ; sh_type      : 32 : endian(endian), bind(read_sh_type sh_type)
          ; sh_flags     : 64 : bitstring
          ; sh_addr      : 64 : endian(endian)
          ; sh_offset    : 64 : endian(endian)
          ; sh_size      : 64 : endian(endian)
          ; sh_link      : 32 : endian(endian)
          ; sh_info      : 32 : endian(endian)
          ; sh_addralign : 64 : endian(endian)
          ; sh_entsize   : 64 : endian(endian)
          } ->
            { sh_name
            ; sh_type
            ; sh_flags
            ; sh_addr
            ; sh_offset
            ; sh_size
            ; sh_link
            ; sh_info
            ; sh_addralign
            ; sh_entsize
            ; name = "" (* the name is found in a second pass *)
            }
      in
      let e_shdr_array = Array.init e_hdr.e_shnum read_nth in
      (* Now we can fill the "name" field *)
      let find_name n =
        let strtab_section = e_shdr_array.(e_hdr.e_shstrndx) in
        let strtab_bit_start = Safe.(8 * of_int64 strtab_section.sh_offset) in
        let strtab_bit_length = Safe.(8 * of_int64 strtab_section.sh_size) in
        let strtab_bitstring =
          Bitstring.subbitstring bs strtab_bit_start strtab_bit_length in
        (* Hack: we exploit the representation of bitstrings, plus the
           alignment constraints, to extract the names... *)
        let (str, ofs, _) = strtab_bitstring in
        let start = ofs / 8 + n in
        String.sub str start (String.index_from str start '\000' - start)
      in
      Array.map
        (fun shdr ->
          { shdr with name = find_name (Int32.to_int shdr.sh_name) }
        )
        e_shdr_array

    let to_string sh =
      Printf.sprintf
        "
{ sh_name      = %s -> %s
; sh_type      = %s
; sh_flags     = %s
; sh_addr      = %s
; sh_offset    = %s
; sh_size      = %s
; sh_link      = %s
; sh_info      = %s
; sh_addralign = %s
; sh_entsize   = %s
}"
        (string_of_int32_d   sh.sh_name) sh.name
        (string_of_sh_type   sh.sh_type)
        (string_of_bitstring sh.sh_flags)
        (string_of_int64_x   sh.sh_addr)
        (string_of_int64_x   sh.sh_offset)
        (string_of_int64_x   sh.sh_size)
        (string_of_int32_x   sh.sh_link)
        (string_of_int32_x   sh.sh_info)
        (string_of_int64_x   sh.sh_addralign)
        (string_of_int64_x   sh.sh_entsize)

  end
end
