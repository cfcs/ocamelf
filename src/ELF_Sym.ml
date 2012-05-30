open Library

    type st_bind =
      | STB_LOCAL
      | STB_GLOBAL
      | STB_WEAK
      | STB_OS     of int
      | STB_PROC   of int
      | STB_OTHER  of int

    let string_of_st_bind = function
    | STB_LOCAL    -> "STB_LOCAL"
    | STB_GLOBAL   -> "STB_GLOBAL"
    | STB_WEAK     -> "STB_WEAK"
    | STB_OS(x)    -> "STB_OS("    ^ string_of_int x ^ ")"       
    | STB_PROC(x)  -> "STB_PROC("  ^ string_of_int x ^ ")"
    | STB_OTHER(x) -> "STB_OTHER(" ^ string_of_int x ^ ")"

    let (read_st_bind, write_st_bind) =
      let (read_st_bind, write_st_bind) = mk_rw
        [ (0, STB_LOCAL )
        ; (1, STB_GLOBAL)
        ; (2, STB_WEAK  )
        ]
      in
      (
        (fun x ->
          if 10 <= x && x <= 12
          then STB_OS(x)
          else if 13 <= x && x <= 15
          then STB_PROC(x)
          else
            try read_st_bind x
            with Not_found -> STB_OTHER(x)
        ),
        (function
        | STB_OS(x)    -> x
        | STB_PROC(x)  -> x
        | STB_OTHER(x) -> x
        | x           -> write_st_bind x
        )
      )

    type st_type =
      | STT_NOTYPE
      | STT_OBJECT
      | STT_FUNC
      | STT_SECTION
      | STT_FILE
      | STT_OS      of int
      | STT_PROC    of int
      | STT_OTHER   of int

    let string_of_st_type = function
      | STT_NOTYPE   -> "STT_NOTYPE"
      | STT_OBJECT   -> "STT_OBJECT"
      | STT_FUNC     -> "STT_FUNC"
      | STT_SECTION  -> "STT_SECTION"
      | STT_FILE     -> "STT_FILE"
      | STT_OS(x)    -> "STT_OS("    ^ string_of_int x ^ ")"
      | STT_PROC(x)  -> "STT_PROC("  ^ string_of_int x ^ ")"
      | STT_OTHER(x) -> "STT_OTHER(" ^ string_of_int x ^ ")"

    let (read_st_type, write_st_type) =
      let (read_st_type, write_st_type) = mk_rw
        [ (0, STT_NOTYPE )
        ; (1, STT_OBJECT )
        ; (2, STT_FUNC   )
        ; (3, STT_SECTION)
        ; (4, STT_FILE   )
        ]
      in
      (
        (fun x ->
          if 10 <= x && x <= 12
          then STT_OS(x)
          else if 13 <= x && x <= 15
          then STT_PROC(x)
          else
            try read_st_type x
            with Not_found -> STT_OTHER(x)
        ),
        (function
        | STT_OS(x)    -> x
        | STT_PROC(x)  -> x
        | STT_OTHER(x) -> x
        | x           -> write_st_type x
        )
      )


module Make
(INT : INT_t)
(B : sig val size : int end)
(Ehdr : module type of ELF_Ehdr.Make(INT)(B))
(Shdr : module type of ELF_Shdr.Make(INT)(B)(Ehdr))
: sig
    type elf_sym =
        { st_name     : int32
        ; st_bind     : st_bind
        ; st_type     : st_type
        ; st_other    : int
        ; st_shndx    : int
        ; st_value    : INT.t
        ; st_size     : INT.t
        (* extra: *)
        ; st_name_str : string
    }
  val read : Ehdr.elf_ehdr -> Shdr.elf_shdr array -> bitstring -> elf_sym array
end
= struct
    type elf_sym =
        { st_name     : int32
        ; st_bind     : st_bind
        ; st_type     : st_type
        ; st_other    : int
        ; st_shndx    : int
        ; st_value    : INT.t
        ; st_size     : INT.t
        (* extra: *)
        ; st_name_str : string
        }

    module SafeX = Safe.Make(INT)(B)

    let read (ehdr: Ehdr.elf_ehdr) (shdr_array: Shdr.elf_shdr array) (bs: bitstring)
        : elf_sym array =
      let endian = ehdr.endian in
      let symtab_shdr_opt = Shdr.find_by_name_str shdr_array ".symtab" in
      if symtab_shdr_opt = None then
        Array.of_list []
      else
      let Some symtab_shdr = symtab_shdr_opt in
      let sym_byte_size = 24 in
      let nb_syms = SafeX.to_int symtab_shdr.sh_size / sym_byte_size in
      let find_name =
        Shdr.find_name_in_strtab shdr_array.(Int32.to_int symtab_shdr.sh_link) bs
      in
      let read_nth n =
        let sym_bit_ofs = Safe.(
          8 * (SafeX.(to_int symtab_shdr.sh_offset) + n * sym_byte_size)
        ) in
        bitmatch Bitstring.dropbits sym_bit_ofs bs with
          { st_name  : 32 : endian(endian)
          ; st_bind  :  4 : endian(endian), bind(read_st_bind st_bind)
          ; st_type  :  4 : endian(endian), bind(read_st_type st_type)
          ; st_other :  8 : endian(endian)
          ; st_shndx : 16 : endian(endian)
          ; st_value : B.size : bitstring, bind(SafeX.(read_word st_value)|>fst)
          ; st_size  : B.size : bitstring, bind(SafeX.(read_word st_size)|>fst)
          } ->
            { st_name     = st_name
            ; st_bind     = st_bind
            ; st_type     = st_type
            ; st_other    = st_other
            ; st_shndx    = st_shndx
            ; st_value    = st_value
            ; st_size     = st_size
            ; st_name_str = find_name (Int32.to_int st_name)
            }
      in
      Array.init nb_syms read_nth

    let to_string sym =
      Printf.sprintf
        "
{ st_name  = %s -> %s
; st_bind  = %s
; st_type  = %s
; st_other = %s
; st_shndx = %s
; st_value = %s
; st_size  = %s
}"
        (Int32.to_string   sym.st_name ) (* -> *) sym.st_name_str
        (string_of_st_bind sym.st_bind )
        (string_of_st_type sym.st_type )
        (string_of_int     sym.st_other)
        (string_of_int     sym.st_shndx)
        (SafeX.to_string_x sym.st_value)
        (SafeX.to_string_x sym.st_size )

end
