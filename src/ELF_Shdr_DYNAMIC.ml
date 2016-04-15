(* DT_: dynamic section entry constants in (SHT_DYNAMIC) *)

(* the functions below relate to the parsing of SHT_DYNAMIC sections *)
(* readelf -d /usr/bin/id *)
(* Elf(32/64)_Sxword = int64_t *)
(* typedef __u64   Elf64_Xword; *)
(* typedef __u64   Elf64_Addr; *)
(* this has to deal with Elf32_Dyn and Elf64_Dyn
   which is three words { d_tag ; d_val ; d_ptr },
   with d_tag denoting the type,
   and d_val and d_ptr having special meaning
   depending on the context of d_type
   *)

open Library
open Safe

module D_tag
(INT : INT_t)
= struct
type d_tag =
| DT_PROC of INT.t (* range between DT_LOPROC and DT_HIPROC *)
| DT_unknown of INT.t
| DT_OS of INT.t (* range between DT_LOOS and DT_HIOS*)
| DT_NULL     (* no-op, marks the end of a SHT_DYNAMIC array/section. *)
| DT_NEEDED   (* offset to a string in the section referenced in DT_STRTAB, containing the name of required/needed shared object to load, eg "libc.so.6" *)
| DT_PLTRELSZ (* d_val *)
| DT_PLTGOT (* d_ptr *)
| DT_HASH (* d_ptr ; points to "symbol hash table"*)
| DT_STRTAB (* d_ptr; section with list of asciiz strings, usually ".dynstr" *)
| DT_SYMTAB (* d_ptr ; array of symbols (Elf32_Sym or Elf64_Sym) *)
| DT_RELA (* d_ptr ; points to a relocation table, array of Elf32_Rela/Elf64_Rela*)
| DT_RELASZ (* d_val ; total byte size of all DT_RELA sections combined *)
| DT_RELAENT (* d_val ; sizeof Elf32_Rela or Elf64_Rela *)
| DT_STRSZ (* d_val ; total byte size of the DT_STRTAB *)
| DT_SYMENT (* d_val ; sizeof Elf32_Sym or Elf64_Sym *)
| DT_INIT   (* d_ptr ; pointer to initialization function (like _start or main) *)
| DT_FINI   (* d_ptr ; pointer to finalization function (like _exit) *)
| DT_INIT_ARRAY (* d_ptr; to section with array of constructor functions, usually in ".init_array" or ".ctors" *)
| DT_FINI_ARRAY (* d_ptr; like DT_INIT_ARRAY, but for finalizers. usually in ".fini_array" or ".dtors" *)
| DT_INIT_ARRAYSZ (* d_val ; size of DT_INIT_ARRAY array *)
| DT_FINI_ARRAYSZ (* d_val ; size of DT_FINI_ARRAY array *)
| DT_SONAME (* d_val ; offset in the string table (like DT_NEEDED), used to identify this shared object (if it's a .so) by name *)
| DT_RPATH (* d_val ; sometimes aka DT_RUNPATH *)
| DT_SYMBOLIC
| DT_REL (* d_ptr *)
| DT_RELSZ (* d_val *)
| DT_RELENT (* d_val *)
| DT_PLTREL (* d_val *)
| DT_DEBUG (* d_ptr *)
| DT_TEXTREL
| DT_JMPREL (* d_ptr *)
| DT_ENCODING
| DT_VALRNGLO
| DT_VALRNGHI
| DT_ADDRRNGLO
| DT_ADDRRNGHI
| DT_GNU_HASH
| DT_VERSYM
| DT_RELACOUNT
| DT_RELCOUNT
| DT_FLAGS_1
| DT_VERDEF
| DT_VERDEFNUM
| DT_VERNEED
| DT_VERNEEDNUM
(*| OLD_DT_HIOS*)
(*| OLD_DT_LOOS*)

let dt_table_template = 
[   (DT_NULL    , "DT_NULL"     , 0)
  ; (DT_NEEDED  , "DT_NEEDED"   , 1)
  ; (DT_PLTRELSZ, "DT_PLTRELSZ" , 2)
  ; (DT_PLTGOT  , "DT_PLTGOT"   , 3)
  ; (DT_HASH    , "DT_HASH"     , 4)
  ; (DT_STRTAB  , "DT_STRTAB"   , 5)
  ; (DT_SYMTAB  , "DT_SYMTAB"   , 6)
  ; (DT_RELA    , "DT_RELA"     , 7)
  ; (DT_RELASZ  , "DT_RELASZ"   , 8)
  ; (DT_RELAENT , "DT_RELAENT"  , 9)
  ; (DT_STRSZ   , "DT_STRSZ"    , 10)
  ; (DT_SYMENT  , "DT_SYMENT"   , 11)
  ; (DT_INIT    , "DT_INIT"     , 12)
  ; (DT_FINI    , "DT_FINI"     , 13)
  ; (DT_SONAME  , "DT_SONAME"   , 14)
  ; (DT_RPATH   , "DT_RPATH "   , 15)
  ; (DT_SYMBOLIC, "DT_SYMBOLIC" , 16)
  ; (DT_REL     , "DT_REL"      , 17)
  ; (DT_RELSZ   , "DT_RELSZ"    , 18)
  ; (DT_RELENT  , "DT_RELENT"   , 19)
  ; (DT_PLTREL  , "DT_PLTREL"   , 20)
  ; (DT_DEBUG   , "DT_DEBUG"    , 21)
  ; (DT_TEXTREL , "DT_TEXTREL"  , 22)
  ; (DT_JMPREL  , "DT_JMPREL"   , 23)
  ; (DT_INIT_ARRAY, "DT_INIT_ARRAY", 0x19)
  ; (DT_FINI_ARRAY, "DT_FINI_ARRAY", 0x1a)
  ; (DT_INIT_ARRAYSZ, "DT_INIT_ARRAYSZ", 0x1b)
  ; (DT_FINI_ARRAYSZ, "DT_FINI_ARRAYSZ", 0x1c)
  ; (DT_ENCODING, "DT_ENCODING" , 32)
(*; (OLD_DT_LOOS, "OLD_DT_LOOS" , 0x600000_00)*)
(*; (OLD_DT_HIOS, "OLD_DT_HIOS" , 0x6fff_ff_ff) *)
(*  ; (DT_LOOS    , "DT_LOOS"     , 0x600000_0d)
  ; (DT_HIOS    , "DT_HIOS"     , 0x6fff_f0_00)
*)
  ; (DT_VALRNGLO , "DT_VALRNGLO" , 0x6fff_fd_00)
  ; (DT_VALRNGHI , "DT_VALRNGHI" , 0x6fff_fd_ff)
  ; (DT_ADDRRNGLO, "DT_ADDRRNGLO", 0x6fff_fe_00)
    ; (DT_GNU_HASH , "DT_GNU_HASH" , 0x6fff_fe_f5)
  ; (DT_ADDRRNGHI, "DT_ADDRRNGHI", 0x6fff_fe_ff)
  ; (DT_VERSYM   , "DT_VERSYM"   , 0x6fff_ff_f0)
  ; (DT_RELACOUNT, "DT_RELACOUNT", 0x6fff_ff_f9)
  ; (DT_RELCOUNT , "DT_RELCOUNT" , 0x6fff_ff_fa)
  ; (DT_FLAGS_1  , "DT_FLAGS_1"  , 0x6fff_ff_fb)
  ; (DT_VERDEF   , "DT_VERDEF"   , 0x6fff_ff_fc)
  ; (DT_VERDEFNUM, "DT_VERDEFNUM", 0x6fff_ff_fd)
  ; (DT_VERNEED  , "DT_VERNEED"  , 0x6fff_ff_fe)
  ; (DT_VERNEEDNUM,"DT_VERNEEDNUM", 0x6fff_ff_ff)
(*  ; (DT_LOPROC   , "DT_LOPROC"   , 0x70000000)
  ; (DT_HIPROC   , "DT_HIPROC"   , 0x7fffffff)
*)
]

  let dt_table =
    let module SafeX = Safe.Make(INT) in
    dt_table_template |> List.map (fun (a,b,c) -> (a,b,SafeX.of_int c))
end

(* end of DT_: dynamic section entry constants *)

