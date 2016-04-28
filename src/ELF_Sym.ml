open Library

type st_bind = (* symbol table bind type constants*)
| STB_LOCAL
| STB_GLOBAL (* globally visible *)
| STB_WEAK (* other definitions of this symbol take precedence *)
| STB_OS     of int (* in the range STB_LOOS / STB_HIOS , reserved *)
| STB_PROC   of int (* in the range STB_LOPROC / STB_HIPROC, reserved *)
| STB_OTHER  of int (* reserved *)

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

type st_type = (* symbol table entry type constants *)
| STT_NOTYPE  (* type not specified *)
| STT_OBJECT  (* data object (variable, array, etc) *)
| STT_FUNC    (* function*)
| STT_SECTION (* symbol refers to a section *)
| STT_FILE    (* local, absolute symbol that refers to a file *)
| STT_COMMON  (* an uninitialized common block *)
| STT_TLS     (* thread local storage object *)
| STT_GNU_IFUNC (* GNU indirect function - see http://willnewton.name/uncategorized/using-gnu-indirect-functions/ - also used for STT_AMDGPU_HSA_KERNEL *)
| STT_OS      of int
| STT_PROC    of int
| STT_OTHER   of int

let string_of_st_type = function
| STT_NOTYPE   -> "STT_NOTYPE"
| STT_OBJECT   -> "STT_OBJECT"
| STT_FUNC     -> "STT_FUNC"
| STT_SECTION  -> "STT_SECTION"
| STT_FILE     -> "STT_FILE"
| STT_COMMON   -> "STT_COMMON"
| STT_TLS      -> "STT_TLS"
| STT_GNU_IFUNC -> "STT_GNU_IFUNC"
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
   ; (5, STT_COMMON ) 
   ; (6, STT_TLS    )
   ; (10, STT_GNU_IFUNC )
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

module Elf_Sym_t (INT : INT_t) =
struct
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
end

module Make
(INT : INT_t)
(Ehdr : module type of ELF_Ehdr.Make(INT))
(Shdr : module type of ELF_Shdr.Make(INT))
: sig
  include module type of Elf_Sym_t(INT)
  val read : Ehdr.elf_ehdr -> Shdr.elf_shdr array -> bitstring -> (elf_sym -> bitstring option) -> (elf_sym array) * bitstring
  val to_string : elf_sym -> string
  val make_weak_bind : Ehdr.elf_ehdr -> elf_sym -> bitstring
end = struct
  include Elf_Sym_t(INT)

  module SafeX = Safe.Make(INT)

    let rewrite_sym start bs word =
       let len   = Bitstring.bitstring_length word in
       let first = Bitstring.takebits (start) bs
       and rest  = Bitstring.dropbits (start + len) bs
       in
       Bitstring.concat [first; word; rest]

    let read (ehdr: Ehdr.elf_ehdr) (shdr_array: Shdr.elf_shdr array) (bs: bitstring) (callback : elf_sym -> bitstring option)
        : elf_sym array * bitstring =
      let new_bs = ref bs in
      let endian = ehdr.Ehdr.endian in
      let symtab_shdr_opt = Shdr.find_by_name_str shdr_array ".dynsym" (*".symtab"*) in
      if symtab_shdr_opt = None then
        Array.of_list [] , bs
      else
      let symtab_shdr = match symtab_shdr_opt with Some e -> e | None -> failwith "check three lines above, this cannot be None." in
      let sym_byte_size = 24 in
      let nb_syms = SafeX.to_int symtab_shdr.Shdr.sh_size / sym_byte_size in
      let find_name =
        Shdr.find_name_in_strtab shdr_array.(Int32.to_int symtab_shdr.Shdr.sh_link) bs
      in
      let read_nth n =
        let sym_bit_ofs = Safe.(
          8 * (SafeX.(to_int symtab_shdr.Shdr.sh_offset) + n * sym_byte_size)
        ) in
        bitmatch Bitstring.dropbits sym_bit_ofs bs with
          { st_name  : 32 : endian(endian)
          ; st_bind  :  4 : endian(endian), bind(read_st_bind st_bind)
          ; st_type  :  4 : endian(endian), bind(read_st_type st_type)
          ; st_other :  8 : endian(endian)
          ; st_shndx : 16 : endian(endian)
          ; st_value : INT.wordsize : bitstring, bind(SafeX.(read_word st_value)|>fst)
          ; st_size  : INT.wordsize : bitstring, bind(SafeX.(read_word st_size)|>fst)
          } ->
            let sym =
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
              let () =
                begin match callback sym with
                | None -> ()
                | Some modified_bs ->
                    assert ( Bitstring.(bitstring_length modified_bs) / 8 = INT.to_int symtab_shdr.Shdr.sh_entsize) ;
                    new_bs := rewrite_sym sym_bit_ofs !new_bs modified_bs
                end
              in sym
      in
      let parsed_syms = Array.init nb_syms read_nth in
      (parsed_syms, !new_bs)

    let to_bitstring (ehdr : Ehdr.elf_ehdr) sym =
      let endian = ehdr.Ehdr.endian in
      BITSTRING {
        sym.st_name : 32 : endian(endian)
      ; write_st_bind sym.st_bind :  4 : endian(endian)
      ; write_st_type sym.st_type :  4 : endian(endian)
      ; sym.st_other:  8 : endian(endian)
      ; sym.st_shndx: 16 : endian(endian)
      ; SafeX.to_bitstring sym.st_value: INT.wordsize : bitstring
      ; SafeX.to_bitstring sym.st_size : INT.wordsize : bitstring
      }

    let make_weak_bind ehdr (sym : elf_sym) =
      { sym with st_bind = STB_WEAK } |> to_bitstring ehdr

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
