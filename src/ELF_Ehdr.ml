open ELF_Identification
open Library

type em =
  | EM_NONE
  | EM_M32
  | EM_SPARC
  | EM_386
  | EM_68K
  | EM_88K
  | EM_860
  | EM_MIPS
  | EM_MIPS_RS4_BE
  | EM_PPC
  | EM_X86_64

let (read_em, write_em) = mk_rw
  [ ( 0, EM_NONE)
  ; ( 1, EM_M32)
  ; ( 2, EM_SPARC)
  ; ( 3, EM_386)
  ; ( 4, EM_68K)
  ; ( 5, EM_88K)
  ; ( 7, EM_860)
  ; ( 8, EM_MIPS)
  ; (10, EM_MIPS_RS4_BE)
  ; (20, EM_PPC)
  ; (62, EM_X86_64)
  ]

let string_of_em = function
| EM_NONE           -> "EM_NONE"
| EM_M32            -> "EM_M32"
| EM_SPARC          -> "EM_SPARC"
| EM_386            -> "EM_386"
| EM_68K            -> "EM_68K"
| EM_88K            -> "EM_88K"
| EM_860            -> "EM_860"
| EM_MIPS           -> "EM_MIPS"
| EM_MIPS_RS4_BE    -> "EM_MIPS_RS4_BE"
| EM_PPC            -> "EM_PPC"
| EM_X86_64         -> "EM_X86_64"

type ev =
  | EV_NONE
  | EV_CURRENT

let (read_ev, write_ev) = mk_rw
  [ (0l, EV_NONE)
  ; (1l, EV_CURRENT)
  ]

let string_of_ev = function
| EV_NONE       -> "EV_NONE"
| EV_CURRENT    -> "EV_CURRENT"

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


module Make
(INT : INT_t )
(B : sig val size : int end)
: sig
    type elf_ehdr =
        { e_ident     : elf_identification
        ; e_type      : et
        ; e_machine   : em
        ; e_version   : ev
        ; e_entry     : INT.t
        ; e_phoff     : INT.t
        ; e_shoff     : INT.t
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
  val read : elf_identification -> bitstring -> elf_ehdr
  val to_string : elf_ehdr -> string
end
=
  struct
    type elf_ehdr =
        { e_ident     : elf_identification
        ; e_type      : et
        ; e_machine   : em
        ; e_version   : ev
        ; e_entry     : INT.t
        ; e_phoff     : INT.t
        ; e_shoff     : INT.t
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

    module SafeX = Safe.Make (INT) (B)

    open ELF_Identification

    let read (e_ident: elf_identification) (bs: bitstring): elf_ehdr =
      let endian = endian_of_ei_data e_ident.ei_data in
      bitmatch Bitstring.dropbits 128 bs with
        { e_type      : 16 : int, endian(endian), bind (read_et e_type)
        ; e_machine   : 16 : int, endian(endian), bind (read_em e_machine)
	; e_version   : 32 : int, endian(endian), bind (read_ev e_version)
        ; e_entry     : B.size : bitstring, bind (SafeX.read_word e_entry |> fst)
	; e_phoff     : B.size : bitstring, bind (SafeX.read_word e_phoff |> fst)
        ; e_shoff     : B.size : bitstring, bind (SafeX.read_word e_shoff |> fst)
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
        (ELF_Identification.to_string eh.e_ident    )
        (string_of_et             eh.e_type     )
        (string_of_em             eh.e_machine  )
        (string_of_ev             eh.e_version  )
        (SafeX.to_string_x          eh.e_entry    )
        (SafeX.to_string_x          eh.e_phoff    )
        (SafeX.to_string_x          eh.e_shoff    )
        (string_of_bitstring      eh.e_flags    )
        (string_of_int            eh.e_ehsize   )
        (string_of_int            eh.e_phentsize)
        (string_of_int            eh.e_phnum    )
        (string_of_int            eh.e_shentsize)
        (string_of_int            eh.e_shnum    )
        (string_of_int            eh.e_shstrndx )

  end
