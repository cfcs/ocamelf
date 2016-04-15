open ELF
open ELF_Identification
open Library

let _ =
  let bs = Bitstring.bitstring_of_file Sys.argv.(1) in
  let module INT = (val ELF.infer_elf_int_type bs) in
  let module ELF = ELF.Make(INT) in
  let ei = ELF_Identification.read bs in
  let eh = ELF.Ehdr.read ei bs in
  let shdr_array = ELF.Shdr.read eh bs in
  (*let phdr_array = ELF.Phdr.read eh bs in*)
  let (sym_array , bs) = ELF.Sym.read eh shdr_array bs
    (fun sym ->
     let open ELF_Sym in
     let open ELF.Sym in
     if sym.st_bind = STB_GLOBAL && sym.st_type = STT_FUNC then
       (* make it STB_WEAK if it is currently a global function *)
       Some (ELF.Sym.make_weak_bind eh sym)
     else
       None
    )
  in
  print_endline (ELF.Ehdr.to_string eh);
  let new_bs = ref bs in
  let handle_shdr (shdr : ELF.Shdr.elf_shdr) =
    if ELF.Shdr.sh_type_of_shdr shdr = Library.SHT_DYNAMIC then
      ( (* going to add a DT_NEEDED entry *)
      print_endline (ELF.Shdr.to_string shdr)
      ;
      let (dt_entry, modified_bs) =
        ELF.Shdr_DYNAMIC.strtab_entry_from_offset shdr !new_bs shdr_array 1
      in
      List.iter print_endline dt_entry
      ; new_bs := modified_bs
      )
    else
      () (* print_endline (ELF.Shdr.to_string shdr) *)
  in
  Array.iter (handle_shdr) shdr_array ;
  (*Array.iter (fun phdr -> print_endline (ELF.Phdr.to_string phdr)) phdr_array;*)
  Array.iter (fun sym -> print_endline (ELF.Sym.to_string sym)) sym_array
  ; Printf.printf "Writing new file to /tmp/yolo\n"
  ; Bitstring.bitstring_to_file !new_bs "/tmp/yolo"
