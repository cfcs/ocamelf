open Library

(* SHT_* : section header tables *)

type sh_type =
      | SHT_NULL            (* no section associated with header *)
      | SHT_PROGBITS        (* program-defined data *)
      | SHT_SYMTAB          (* link editing and dynamic linking symbols *)
      | SHT_STRTAB          (* string table *)
      | SHT_RELA            (* minimal set of linking symbols *)
      | SHT_HASH            (* relocation entries with explicit address *)
      | SHT_DYNAMIC         (* symbol hash table (dynamic linking) *)
      | SHT_NOTE            (* dynamic linking info *)
      | SHT_NOBITS          (* file-marking information *)
      | SHT_REL             (* relocation entries without explicit address *)
      | SHT_SHLIB           (* reserved *)
      | SHT_DYNSYM          (* dynamic linking symbol table *)
      | SHT_PROC  of int32  (* processor-specific dynamics *)
      | SHT_USER  of int32  (* application-specific dynamics *)
      | SHT_OTHER of int32  (* not defined in ELF32 or ELF64 *)
      (* when merging there was reference to 0x6000_0000 - 0x6fff_ffff
         as "SHT_OS", but while
         OLD_DT_LOOS (0x6000_0000) / DT_LOOS (0x6000_00_0d)
                                   / DT_HIOS (0x6fff_f000)
         and PT_LOOS (0x60000000) / PT_HIOS (0x6fffffff)
         are mentioned in the spec, I couldn't find any mention of
         SHT_OS/SHT_LOOS/SHT_HIOS, so I took that out *)

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
    | SHT_PROC(x)  -> Int32.format "SHT_PROC(%08x)" x
    | SHT_USER(x)    -> Int32.format "SHT_USER(%08x)" x
    | SHT_OTHER(x) -> Int32.format "SHT_OTHER(%08x)" x

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
          if 0x7000_0000l <= x && x <= 0x7FFF_FFFFl
          then SHT_PROC(x)
          else if 0x8000_0000l <= x && x <= 0xFFFF_FFFFl
          then SHT_USER(x)
          else
            try read_sh_type x
            with Not_found -> SHT_OTHER(x)
        ),
        (function
        | SHT_PROC(x)  -> x
        | SHT_USER(x)  -> x
        | SHT_OTHER(x) -> x
        | x            -> write_sh_type x
        )
      )

(* end of SHT_ : section header tables *)



(* the functor below handles Elf32 and Elf64 differences using
   either Int32 or Int64. We additionally need to know the word size
   of INT.t, so the user must pass that in.
   example: Make(Int32)(struct let size = 32 end *)

module Make
(INT : INT_t)
(B : sig val size : int end)
(Ehdr_INT : module type of ELF_Ehdr.Make(INT)(B))
: sig
  type elf_shdr =
        { sh_name      : int32 (* byte offset to section name *)
        ; sh_type      : sh_type (* identifies the section type *)
        ; sh_flags     : bitstring (* identifies section attributes*)
        ; sh_addr      : INT.t (* virtual address of beginning of section *)
        ; sh_offset    : INT.t (* byte offset to beginning of section (in this file) *)
        ; sh_size      : INT.t (* byte length of section in file, except for sh_type=SHT_NOBITS *)
        ; sh_link      : int32 (* section index of an associated section *)
          (* SHT_HASH: symbol table to which the hash table applies *)
          (* SHT_REL(A): symbol table referenced by relocations*)
          (* SHT_DYNAMIC/SHT_SYMTAB/SHT_DYNSYM: string table used by entries in this section *)
        ; sh_info      : int32 (* "extra information" about section *)
        ; sh_addralign : INT.t (* required alignment of section, always a power of two *)
        ; sh_entsize   : INT.t (* sizeof(elements) if section contains fixed-size elements, otherwise 0 *)
        (* not stored in file, but provided for ease of library use: *)
        ; sh_name_str  : string
    }
val find_by_name_str : elf_shdr array -> string -> elf_shdr option
val find_name_in_strtab : elf_shdr -> Bitstring.bitstring -> int -> string
val read : Ehdr_INT.elf_ehdr -> Bitstring.bitstring -> elf_shdr array
val sh_type_of_shdr : elf_shdr -> sh_type
val to_string : elf_shdr -> string
  val strtab_entry_from_offset : elf_shdr -> Bitstring.bitstring -> elf_shdr array -> int -> string list
end
= struct
module SafeX = Safe.Make(INT)(B)

(* "d_val" is an offset in the string table .dynstr to name of a needed library*)
(* readelf -d /usr/bin/id *)
(* Elf(32/64)_Sxword = int64_t *)
(* typedef __u64   Elf64_Xword; *)
(* typedef __u64   Elf64_Addr; *)
let const_DT_NEEDED : INT.t = SafeX.of_int 1

(* see https://uclibc.org/docs/elf-64-gen.pdf *)
    type elf_shdr =
        { sh_name      : int32 (* byte offset to section name *)
        ; sh_type      : sh_type (* identifies the section type *)
        ; sh_flags     : bitstring (* identifies section attributes*)
        ; sh_addr      : INT.t (* virtual address of beginning of section *)
        ; sh_offset    : INT.t (* byte offset to beginning of section (in this file) *)
        ; sh_size      : INT.t (* byte length of section in file, except for sh_type=SHT_NOBITS *)
        ; sh_link      : int32 (* section index of an associated section *)
          (* SHT_HASH: symbol table to which the hash table applies *)
          (* SHT_REL(A): symbol table referenced by relocations*)
          (* SHT_DYNAMIC/SHT_SYMTAB/SHT_DYNSYM: string table used by entries in this section *)
        ; sh_info      : int32 (* "extra information" about section *)
        ; sh_addralign : INT.t (* required alignment of section, always a power of two *)
        ; sh_entsize   : INT.t (* sizeof(elements) if section contains fixed-size elements, otherwise 0 *)
        (* not stored in file, but provided for ease of library use: *)
        ; sh_name_str  : string
}

    let sh_type_of_shdr shdr = shdr.sh_type

    let get_section_body shdr bs =
      let section_bit_start = SafeX.(shdr.sh_offset * of_int 8 |> to_int) in
      let section_bit_length = SafeX.(shdr.sh_size * of_int 8 |> to_int) in
      Bitstring.subbitstring bs section_bit_start section_bit_length

    let find_name_in_strtab strtab_section bs: int -> string =
      let strtab_bitstring = get_section_body strtab_section bs in
        (* Hack: we exploit the representation of bitstrings, plus the
           alignment constraints, to extract the names... *)
      let (str, ofs, _) = strtab_bitstring in
      fun n ->
        let start = ofs / 8 + n in
        String.sub str start (String.index_from str start '\000' - start)

    let read (ehdr: Ehdr_INT.elf_ehdr) (bs: bitstring) =
      let endian = ehdr.endian in
      let read_nth (n: int) =
      let shdr_bit_ofs =
        let sz = n * ehdr.e_shentsize in
        SafeX.(
	       (of_int 8)
	       * (ehdr.e_shoff + of_int sz)
	       |> to_int
        ) in
        bitmatch Bitstring.dropbits shdr_bit_ofs bs with
          { sh_name      : 32 : endian(endian)
          ; sh_type      : 32 : endian(endian), bind(read_sh_type sh_type)
          ; sh_flags     : B.size : bitstring
          ; sh_addr      : B.size : bitstring, bind(SafeX.read_word sh_addr|>fst)
          ; sh_offset    : B.size : bitstring, bind(SafeX.read_word sh_offset|>fst)
          ; sh_size      : B.size : bitstring, bind(SafeX.read_word sh_size|>fst)
          ; sh_link      : 32 : endian(endian)
          ; sh_info      : 32 : endian(endian)
          ; sh_addralign : B.size : bitstring, bind(SafeX.read_word sh_addralign|>fst)
          ; sh_entsize   : B.size : bitstring, bind(SafeX.read_word sh_entsize|>fst)
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
            ; sh_name_str = "" (* the name is found in a second pass *)
            }
      in
      let shdr_array = Array.init ehdr.e_shnum read_nth in
      (* Now we can fill the "name" field *)
      let find_name = find_name_in_strtab shdr_array.(ehdr.e_shstrndx) bs in
      Array.map
        (fun shdr ->
          { shdr with sh_name_str = find_name (Int32.to_int shdr.sh_name) }
        )
        shdr_array

    let find_by_name_str shdr_array name_str =
      match List.find
          (fun shdr -> shdr.sh_name_str = name_str)
          (Array.to_list shdr_array)
      with
      | shdr -> Some shdr
      | exception Not_found -> None

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
        (Int32.to_string     sh.sh_name     ) (* -> *) sh.sh_name_str
        (string_of_sh_type   sh.sh_type     )
        (string_of_bitstring sh.sh_flags    )
        (SafeX.to_string_x   sh.sh_addr     )
        (SafeX.to_string_x   sh.sh_offset   )
        (SafeX.to_string_x   sh.sh_size     )
        (Int32.format "%08x" sh.sh_link     )
        (Int32.format "%08x" sh.sh_info     )
        (SafeX.to_string_x   sh.sh_addralign)
        (SafeX.to_string_x   sh.sh_entsize  )

    let strtab_entry_from_offset shdr bs shdr_array offset =
      let found_c = ref 0 in
      let init = ref 0 in
      let first_null = ref 0 in
      let strtab = shdr_array.(Int32.to_int shdr.sh_link) in
      let () = print_endline (to_string strtab) in
      let rec read_next acc nbs =
        let (s, o, l) = nbs in
        bitmatch nbs with
        | { d_tag : B.size : bitstring, bind(SafeX.(read_word d_tag |> fst|>to_int))
	  ; d_val : B.size : bitstring, bind(SafeX.(read_word d_val |> fst|>to_int))
          ; nbs : -1 : bitstring} ->
            let acc = (begin match d_tag with
            | 0  -> (if 0 = !first_null then first_null := o) ; "DT_NULL"
            | 1  -> let libname = (find_name_in_strtab strtab bs d_val) in
                     let () = found_c := d_val + 2 in
                     "DT_NEEDED," ^ libname ^ ", "^(string_of_int d_val)
            | 12 -> init := o ; "INIT, " ^ (string_of_int !init)
            | 21 -> "DT_DEBUG"
 (*rewrite_word (BITSTRING { const_DT_NEEDED : B.size : littleendian ; INT.one : B.size : littleendian})*)
            | _   -> ("got an unknown d_tag " ^ string_of_int d_tag ^ ", ") 
             end
^ (string_of_int o) ^ ":"^(string_of_int l)
)  :: acc
            in
            read_next acc nbs
        | {  } -> acc
      in
      let rewrite_word word =
        let () = Printf.printf "init: %d first_null: %d \n" !init !first_null in
              let first  = Bitstring.takebits (!init) bs
              and new_init = Bitstring.(subbitstring bs !init 128)
              and middle = Bitstring.(subbitstring bs (!init + 128) (!first_null - !init -128))
              and rest  = Bitstring.(dropbits (!first_null + 128) bs )
              in
              let concatenated = Bitstring.concat [first; word; new_init ; middle; rest ] in
              Bitstring.bitstring_to_file concatenated "/tmp/id.x"
      in
      let res =
      read_next [] Bitstring.(subbitstring bs (INT.(to_int shdr.sh_offset) * 8) (INT.(to_int shdr.sh_size) * 8))
      |> List.rev in
let () = rewrite_word (
Bitstring.concat [ SafeX.(to_bitstring @@ of_int 1) ; SafeX.(to_bitstring @@ of_int !found_c) ]
      ) in res

end
