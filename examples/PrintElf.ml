open ELF
open ELF_Identification

module Bit32 : sig val size : int end = struct let size = 32 end
module Bit64 : sig val size : int end = struct let size = 64 end

let _ =
  let bs = Bitstring.bitstring_of_file Sys.argv.(1) in
  let elfclass = infer_elfclass bs in
(*  let module ELF =
    match elfclass with
    | ELFCLASS32   -> ELF.Make(Int32)(Bit32)
    | ELFCLASS64   -> ELF.Make(Int64)(Bit64)
    | ELFCLASSNONE -> failwith "Unsupported ELF class"
  in
*)
  let module ELF = ELF.Make(Int64)(Bit64) in
  let ei = ELF_Identification.read bs in
  let eh = ELF.Ehdr.read ei bs in
  let () = Printf.printf "ehdr read\n" in
  let shdr_array = ELF.Shdr.read eh bs in
  let () = Printf.printf "shdr read\n" in
  let phdr_array = ELF.Phdr.read eh bs in
  let sym_array = ELF.Sym.read eh shdr_array bs in
  print_endline (ELF.Ehdr.to_string eh);
  let handle_shdr (shdr : ELF.Shdr.elf_shdr) =
    if ELF.Shdr.sh_type_of_shdr shdr = SHT_DYNAMIC then
      (
print_endline (ELF.Shdr.to_string shdr)
;
List.iter print_endline
  (ELF.Shdr.strtab_entry_from_offset shdr bs shdr_array 1)
)
(* from `man elf` /\.dynamic
           typedef struct {
               Elf64_Sxword    d_tag; (* look for DT_NEEDED *)
               union {
                   Elf64_Xword d_val;
                   Elf64_Addr  d_ptr;
               } d_un;
           } Elf64_Dyn;
           extern Elf64_Dyn _DYNAMIC[];
*)
    else
      () (* print_endline (ELF.Shdr.to_string shdr) *)
  in
  Array.iter (handle_shdr) shdr_array ;
(*
  Array.iter (fun phdr -> print_endline (ELF.Phdr.to_string phdr)) phdr_array;
  Array.iter (fun sym -> print_endline (ELF.Sym.to_string sym)) sym_array
*)
