open Library

type ei_class =
  | ELFCLASSNONE
  | ELFCLASS32
  | ELFCLASS64

type ei_osabi =
  | ELFOSABI_SYSV
  | ELFOSABI_HPUX
  | ELFOSABI_STANDALONE

type ei_abiversion = int

let string_of_ei_osabi = function
| ELFOSABI_SYSV       -> "ELFOSABI_SYSV"
| ELFOSABI_HPUX       -> "ELFOSABI_HPUX"
| ELFOSABI_STANDALONE -> "ELFOSABI_STANDALONE"

let (read_ei_osabi, write_ei_osabi) = mk_rw
  [ (  0, ELFOSABI_SYSV)
  ; (  1, ELFOSABI_HPUX)
  ; (255, ELFOSABI_STANDALONE)
  ]

let (read_ei_class, write_ei_class) = mk_rw
  [ (0, ELFCLASSNONE)
  ; (1, ELFCLASS32)
  ; (2, ELFCLASS64)
  ]

let string_of_ei_class = function
| ELFCLASSNONE -> "ELFCLASSNONE"
| ELFCLASS32   -> "ELFCLASS32"
| ELFCLASS64   -> "ELFCLASS64"

type ei_data =
  | ELFDATANONE
  | ELFDATA2LSB
  | ELFDATA2MSB

let endian_of_ei_data = function
| ELFDATA2LSB -> Bitstring.LittleEndian
| ELFDATA2MSB -> Bitstring.BigEndian
| _           -> failwith "Unknown endianness"

let (read_ei_data, write_ei_data) = mk_rw
  [ (0, ELFDATANONE)
  ; (1, ELFDATA2LSB)
  ; (2, ELFDATA2MSB)
  ]

let string_of_ei_data = function
| ELFDATANONE -> "ELFDATANONE"
| ELFDATA2LSB -> "ELFDATA2LSB"
| ELFDATA2MSB -> "ELFDATA2MSB"

type ei_version =
  | EV_NONE
  | EV_CURRENT

let (read_ei_version, write_ei_version) = mk_rw
  [ (0, EV_NONE)
  ; (1, EV_CURRENT)
  ]

let string_of_ei_version = function
| EV_NONE    -> "EV_NONE"
| EV_CURRENT -> "EV_CURRENT"

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
        (string_of_ei_class   ei.ei_class     )
        (string_of_ei_data    ei.ei_data      )
        (string_of_ei_version ei.ei_version   )
        (string_of_ei_osabi   ei.ei_osabi     )
        (string_of_int        ei.ei_abiversion)
