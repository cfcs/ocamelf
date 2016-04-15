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
  let phdr_array = ELF.Phdr.read eh bs in
  let (sym_array , _ ) = ELF.Sym.read eh shdr_array bs (fun _ -> None) in
  print_endline (ELF.Ehdr.to_string eh);
  let handle_shdr (shdr : ELF.Shdr.elf_shdr) =
    print_endline (ELF.Shdr.to_string shdr)
  in
  Array.iter (handle_shdr) shdr_array ;
  Array.iter (fun phdr -> print_endline (ELF.Phdr.to_string phdr)) phdr_array;
  Array.iter (fun sym -> print_endline (ELF.Sym.to_string sym)) sym_array
