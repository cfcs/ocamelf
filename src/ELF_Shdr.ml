open Library

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

module rec Make
:
functor (INT : INT_t) -> sig
  include module type of Elf_Shdr_t(INT)

  (*include module type of Elf_Shdr_DYNAMIC(INT)*)
  val find_by_name_str : elf_shdr array -> string -> elf_shdr option
  val find_name_in_strtab : elf_shdr -> Bitstring.bitstring -> int -> string
  val read : ELF_Ehdr.Make(INT).elf_ehdr -> Bitstring.bitstring -> elf_shdr array
  val sh_type_of_shdr : elf_shdr -> sh_type
  val to_string : elf_shdr -> string
end
= functor (INT : INT_t) -> struct

  include Library.Elf_Shdr_t(INT) (* provides type elf_shdr with INT.t *)
  include ELF_Shdr_DYNAMIC.D_tag(INT)
  (*module SHT_Dynamic = Elf_Shdr_DYNAMIC(INT)*)
  module SafeX = Safe.Make(INT)
  module Ehdr_INT = ELF_Ehdr.Make(INT)

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
      let endian = ehdr.Ehdr_INT.endian in
      let read_nth (n: int) =
      let shdr_bit_ofs =
        let sz = n * ehdr.Ehdr_INT.e_shentsize in
        SafeX.( (ehdr.e_shoff + of_int sz) * of_int 8 |> to_int
        ) in
        bitmatch Bitstring.dropbits shdr_bit_ofs bs with
          { sh_name      : 32 : endian(endian)
          ; sh_type      : 32 : endian(endian), bind(read_sh_type sh_type)
          ; sh_flags     : INT.wordsize : bitstring
          ; sh_addr      : INT.wordsize : bitstring, bind(SafeX.read_word sh_addr|>fst)
          ; sh_offset    : INT.wordsize : bitstring, bind(SafeX.read_word sh_offset|>fst)
          ; sh_size      : INT.wordsize : bitstring, bind(SafeX.read_word sh_size|>fst)
          ; sh_link      : 32 : endian(endian)
          ; sh_info      : 32 : endian(endian)
          ; sh_addralign : INT.wordsize : bitstring, bind(SafeX.read_word sh_addralign|>fst)
          ; sh_entsize   : INT.wordsize : bitstring, bind(SafeX.read_word sh_entsize|>fst)
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
      let find_name = find_name_in_strtab shdr_array.(ehdr.Ehdr_INT.e_shstrndx) bs in
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
end

and

Elf_Shdr_DYNAMIC
: functor (INT : INT_t) ->
sig
(*  include module type of ELF_Shdr_DYNAMIC.D_tag(INT)*)
  include module type of Elf_Shdr_t(INT)

  val strtab_entry_from_offset : elf_shdr -> Bitstring.bitstring -> elf_shdr array -> int -> (string list) * bitstring
  end
= functor (INT : INT_t) -> struct
  include ELF_Shdr_DYNAMIC.D_tag(INT)
  include Library.Elf_Shdr_t(INT)
  (*type elf_shdr = Elf_Shdr_t(INT).elf_shdr*)
  let rec tuple_of_d_tag ?(tag) ?(tstr) ?(ival) () : (d_tag * string * INT.t) =
    try
      dt_table |> List.find (fun (t_tag,t_tstr,t_ival) ->
        begin match tag, tstr, ival with
        | Some tag , None , None -> tag = t_tag
        | None , Some tstr, None -> tstr = t_tstr
        | None , None     , Some ival -> ival = t_ival
        | Some tag , Some tstr , None      -> tag = t_tag && tstr = t_tstr
        | Some tag , None      , Some ival -> tag = t_tag && ival = t_ival
        | None     , Some tstr , Some ival -> tstr = t_tstr && ival = t_ival
        | Some _ , Some _ , Some _ -> failwith "TODO Some Some Some"
        | None , None , None -> false
        end
      )
    with
    | Not_found ->
       begin match tag with
       | Some (DT_OS i as tag) -> (tag, "DT_OS("^(INT.to_string i)^")", i)
       | Some (DT_PROC i as tag) -> (tag, "DT_PROC("^(INT.to_string i)^")", i)
       | Some (DT_unknown i as tag) -> (tag, "DT_unknown("^(INT.to_string i)^")", i)
       | Some tag -> (tag, "TODO DT_ not found!", INT.min_int)
       | None ->
         begin match ival with
         | Some ival -> tuple_of_d_tag ~tag:(DT_unknown ival) ()
         | None ->
           failwith "tuple_of_tag: TODO: no tag/int; DT_ from string not implemented"
         end
       end
  
  let d_tag_of_int ival : d_tag =
    let ( t , _ , _) = tuple_of_d_tag ~ival ()
     in t
      
  let int_of_d_tag tag : INT.t =
    let ( _ , _ , i) = tuple_of_d_tag ~tag ()
    in i
    
  let string_of_d_tag tag : string =
    let ( _ , s , _) = tuple_of_d_tag ~tag ()
    in s

  let strtab_entry_from_offset shdr bs shdr_array offset =
    let module SafeX = Safe.Make(INT) in
    let module Elf_Shdr = Make(INT) in
    (* TODO this function currently loops over the DYNAMIC section and injects a DT_NEEDED entry. There should be a module for properly parsing the DYNAMIC section as it is rather complex. *)
    let found_c = ref 0 in
    let init = ref 0 in
    let first_null = ref 0 in
    let strtab : Library.Elf_Shdr_t(INT).elf_shdr = shdr_array.(Int32.to_int shdr.Elf_Shdr.sh_link) in
    let rec read_next acc nbs =
      let (s, o, l) = nbs in
      bitmatch nbs with
      | { d_tag : INT.wordsize : bitstring, bind(SafeX.(read_word d_tag |> fst))
        ; d_val : INT.wordsize : bitstring, bind(SafeX.(read_word d_val |> fst|>to_int))
        ; nbs : -1 : bitstring} ->
          let acc =
            (
            (string_of_d_tag (d_tag_of_int d_tag)) ^ " " ^
            begin match d_tag_of_int d_tag with
            | DT_NULL   -> (if 0 = !first_null then first_null := o) ; ""
            | DT_NEEDED -> let libname = (Elf_Shdr.find_name_in_strtab
strtab
bs
d_val) in
                    let () = found_c := d_val + 2 in
                    libname ^ ", " ^ (string_of_int d_val)
            | DT_INIT -> init := o ; (string_of_int !init)
            | _ -> ""
            end
            ^ (string_of_int o) ^ " : " ^(string_of_int l)
            ) :: acc
          in
            read_next acc nbs
      | {  } -> acc
    in
    let rewrite_word word =
      let word_len = Bitstring.bitstring_length word in
      let () = Printf.printf "init: %d first_null: %d \n" !init !first_null in
      let first  = Bitstring.takebits (!init) bs
      and new_init = Bitstring.(subbitstring bs !init word_len)
      and middle = Bitstring.(subbitstring bs (!init + word_len) (!first_null - !init - word_len))
      and rest  = Bitstring.(dropbits (!first_null + word_len) bs )
      in
      let concatenated = Bitstring.concat [first; word; new_init ; middle; rest ] in
        concatenated
    in
    let res =
    read_next [] Bitstring.(subbitstring bs (INT.(to_int shdr.sh_offset) * 8) (INT.(to_int shdr.sh_size) * 8))
    |> List.rev in
    let modified_bs = rewrite_word (
      Bitstring.concat [ SafeX.(to_bitstring @@ of_int 1) ; SafeX.(to_bitstring @@ of_int !found_c) ]
    )
    in (res , modified_bs)

end
