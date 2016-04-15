(* "Hacker's Delight", section 2.12 *)

let ( + ) x y =
  let z = x + y in
  (* Overflow occurs iff x and y have same sign and z's sign is different *)
  if (z lxor x) land (z lxor y) < 0
  then raise Exc.IntOverflow
  else z

let ( - ) x y =
  let z = x - y in
  (* Overflow occurs iff x and y have opposite signs and z and x have
     opposite signs *)
  if (x lxor y) land (z lxor x) < 0
  then raise Exc.IntOverflow
  else z

let ( * ) x y =
  let z = x * y in
  if (x = min_int && y < 0) || (y <> 0 && z / y <> x)
  then raise Exc.IntOverflow
  else z

open Library

module Make
(INT : INT_t )
: sig
val ( + ) : INT.t -> INT.t -> INT.t
val ( - ) : INT.t -> INT.t -> INT.t
val ( * ) : INT.t -> INT.t -> INT.t
val to_int : INT.t -> int
val of_int : int -> INT.t 
val to_string_x : INT.t -> string
val to_string_d : INT.t -> string
val read_word : Bitstring.bitstring -> INT.t * Bitstring.bitstring
val to_bitstring : INT.t -> Bitstring.bitstring
end
= struct

let read_word (bs : Bitstring.bitstring) =
  let word = Bitstring.takebits INT.wordsize bs in
  let rec countdown acc = function
    | 0 -> acc
    | size ->
       bitmatch word with
       { int_16 : 16 : littleendian, offset(INT.wordsize - size) } ->
         let acc = INT.(add acc
	  (shift_left (of_int int_16) (INT.wordsize - size))
	 )
         in countdown acc (size - 16)
  in
  ( countdown INT.zero INT.wordsize
  , Bitstring.dropbits INT.wordsize bs)

let to_bitstring acc =
  let rec append word = function
    | 0 , acc -> word
    | n , acc ->
      let newx =
      ( let i = INT.to_int acc in
	 Printf.sprintf "%c%c"
	 ( i land 0xff |> char_of_int)
         ((i land 0xff00) lsr 8|>char_of_int)
      )|> Bitstring.bitstring_of_string
      in
      let bword = Bitstring.concat [ word ; newx ] in
      append bword (n-16 , INT.shift_right acc 16)
  in
    let () = Printf.printf "debug: acc = %s\n" INT.(to_string acc)in
    let b  = append Bitstring.empty_bitstring (INT.wordsize, acc) in
    let () = Bitstring.hexdump_bitstring stdout b in
    b

(* To print addresses / offsets *)
let to_string_x i = INT.format "0x%16x" i

(* To print counts/indices *)
let to_string_d i = INT.to_string i

let ( + ) x y = INT.(
  let z = add x y in
  (* Overflow occurs iff x and y have same sign and z's sign is different *)
  if logand (logxor z x) (logxor z y) < zero
  then raise Exc.IntOverflow
  else z
)

let ( - ) x y = INT.(
  let z = sub x y in
  (* Overflow occurs iff x and y have opposite signs and z and x have
     opposite signs *)
  if logand (logxor x y) (logxor z x) < zero
  then raise Exc.IntOverflow
  else z
)

let ( * ) x y = INT.(
  let z = mul x y in
  if (x = min_int && y < zero) || (y <> zero && div z y <> x)
  then
    ( Printf.printf "%s ; %s\n" INT.(to_string x) INT.(to_string y)
    ;raise Exc.IntOverflow
    )
  else z
)

let to_int i32 = INT.(
  let i = to_int i32 in
  if i32 = of_int i
  then i
  else raise Exc.IntOverflow
)

let of_int = INT.of_int

end

let of_int32 = Int32.to_int
let to_int32 = Int32.of_int
