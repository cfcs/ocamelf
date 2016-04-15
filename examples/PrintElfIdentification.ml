open ELF
open ELF_Identification

let _ =
  let bs = Bitstring.bitstring_of_file Sys.argv.(1) in
  let module INT = (val ELF.infer_elf_int_type bs) in
  let module ELF = ELF.Make(INT) in
  let ei = ELF_Identification.read bs in
  print_endline (ELF_Identification.to_string ei)
