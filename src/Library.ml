type bitstring = Bitstring.bitstring

module type INT_t = sig
     type t
     val wordsize : int (* bitsize of the word, eg 32 or 64 *)

     val add : t -> t -> t
     val format : string -> t -> string
     val logand : t -> t -> t
     val logxor : t -> t -> t
     val sub : t -> t -> t
     val mul : t -> t -> t
     val min_int : t
     val div : t -> t -> t
     val to_int : t -> int
     val to_int32 : t -> int32
     val to_int64 : t -> int64
     val to_string : t -> string
     val of_int : int -> t
     val of_int32 : int32 -> t
     val of_int64 : int64 -> t
     val zero : t
     val shift_left : t -> int -> t
     val shift_right : t -> int -> t
end

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
                               / DT_HIOS (0x6fff_f0_00)
     and PT_LOOS (0x60000000)  / PT_HIOS (0x6fff_ff_ff)
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


module Elf_Shdr_t (INT : INT_t) = struct
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
end


let mk_rw l =
  (
    (fun x -> snd (List.find (fun (k, _) -> k = x) l)),
    (fun x -> fst (List.find (fun (_, k) -> k = x) l))
  )

(*module IntMap = Map.Make(struct type t = int let compare = compare end)*)
module StringMap = Map.Make (String)

let rec is_zeros (bs: bitstring): int -> bool = function
| 0 -> true
| bitsize when bitsize >= 64 ->
    (
      bitmatch bs with
      | { 0L : 64 : int ; rest : -1 : bitstring } ->
          is_zeros rest (bitsize - 64)
      | { _ } -> false
    )
| bitsize ->
    (
      bitmatch bs with
      | { 0L : bitsize : int } -> true
      | { _ } -> false
    )

(*
let is_some: 'a option -> bool = function
| Some(_) -> true
| None    -> false

let from_some: 'a option -> 'a = function
| Some(x) -> x
| None    -> raise Not_found

let filter_some (l: 'a option list): 'a list =
  List.(map from_some (filter is_some l))

type 'a or_err =
  | OK of 'a
  | ERR of string

let is_ok: 'a or_err -> bool = function
| OK(_) -> true
| ERR(_) -> false

let is_err x = not (is_ok x)

let from_ok: 'a or_err -> 'a = function
| OK(x) -> x
| ERR(_) -> assert false

let from_err: 'a or_err -> string = function
| OK(_) -> assert false
| ERR(s) -> s

let filter_ok (l: 'a or_err list): 'a list =
  List.(map from_ok (filter is_ok l))

let filter_err (l: 'a or_err list): string list =
  List.(map from_err (filter is_err l))

external id : 'a -> 'a = "%identity"
*)
(** Checks for existence of an array element satisfying a condition, and returns
    its index if it exists.
*)
let array_exists (cond: 'a -> bool) (arr: 'a array): int option =
  let rec array_exists_aux ndx =
    if ndx < 0
    then None
    else if cond arr.(ndx)
    then Some ndx
    else array_exists_aux (ndx - 1)
  in array_exists_aux (Array.length arr - 1)

let string_of_array string_of_elt sep a =
  let contents =
    (fst
       (Array.fold_left
          (fun accu elt ->
            let (str, ndx) = accu in
            (str ^ (if ndx > 0 then sep else "") ^ string_of_int ndx ^ ": " ^
               string_of_elt elt, ndx + 1)
          )
          ("", 0) a
       )
    )
  in "[\n" ^ contents ^ "\n]"
(*
let string_of_list string_of_elt sep l =
  String.concat sep (List.map string_of_elt l)
*)
let string_of_bitstring bs =
  let rec string_of_bitset_aux bs =
    bitmatch bs with
    | { bit  : 1  : int       ;
        rest : -1 : bitstring } ->
        (if bit then "1" else "0") ^ (string_of_bitset_aux rest)
    | { _ } -> ""
  in string_of_bitset_aux bs

let sorted_lookup (compare: 'a -> 'b -> int) (arr: 'a array) (v: 'b): 'a option =
  let rec sorted_lookup_aux (i_from: int) (i_to: int): 'a option =
    if i_from > i_to
    then None
    else
      let i_mid = (i_from + i_to) / 2 in
      let comp = compare arr.(i_mid) v in
      if comp < 0 (* v_mid < v *)
      then sorted_lookup_aux (i_mid + 1) i_to
      else if comp > 0
      then sorted_lookup_aux i_from (i_mid - 1)
      else Some(arr.(i_mid))
  in sorted_lookup_aux 0 (Array.length arr - 1)


let elf_hash name =
  (* "ELF hash" aka "SYSV hash" - NOTE that GNU_HASH sections use djb_hash() *)
  (* 32-bit hash function used in Elf64 (and Elf32?) for hash table lookups.
     The function takes a string and returns int32 to be interpreted as uint32
     see: http://downloads.openwatcom.org/ftp/devel/docs/elf-64-gen.pdf
  *)
  (* reference implementation:
     unsigned long elf_hash(const unsigned char *name)
     { unsigned long h = 0, g;
       while ( *name)
       { h = (h << 4) + *name++;
         if (g = h & 0xf0000000) h ^= g >> 24;
         h &= ~g;
       } return h; }
  *)
  let open Int32 in
  let h = ref 0l
  and g = ref 0l in
  name |> String.iteri
    (fun _ char ->
      h := add (shift_left !h 4) (char |> int_of_char |> of_int)
    ; g := logand !h 0xf0_00_0000_l
    ; if !g <> 0l then
        h := logxor !h (shift_right_logical !g 24)
    ; h := logand !h 0x0f_ff_ffff_l
    )
  ; !h
let sysv_hash = elf_hash

let djb_hash name =
  (* hash function used for string lookups in GNU_HASH sections,
     - NOTE that the hash tables in the ELF specification use elf_hash()
     see: https://blogs.oracle.com/ali/entry/gnu_hash_elf_sections
  *)
  (* reference implementation:
     uint32_t dl_new_hash (const char *s)
     { uint32_t h = 5381;
       for (unsigned char c = *s; c != '\0'; c = *++s)
         h = h * 33 + c;
       return h; }
  *)
  let open Int32 in
  let h = ref 5381_l in
  name |> String.iteri
    (fun _ char ->
      let char = char |> int_of_char |> of_int in
      h := shift_left !h 5 |> add !h |> add char
    )
  ; !h
let dl_new_hash = djb_hash

