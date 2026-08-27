let mul_string s f =
	let i =
		match Float.classify_float f with
		| FP_nan -> 0
		| FP_infinite -> 1_000_000
		| _ -> max 0 (min 1_000_000 (Int.of_float f))
	in
	if i < 1 || String.length s = 0 then ""
	else
		let b = Buffer.create (min Sys.max_string_length (i * String.length s)) in
		for _ = 1 to i do Buffer.add_string b s done;
		Buffer.contents b

let string_of_chars c = String.of_seq (List.to_seq (List.rev c))

let chars_of_string s = List.of_seq (String.to_seq s)

let utf8_chars s =
	let len = String.length s in
	let width c =
		let n = Char.code c in
		if n land 0x80 = 0 then 1
		else if n land 0xe0 = 0xc0 then 2
		else if n land 0xf0 = 0xe0 then 3
		else if n land 0xf8 = 0xf0 then 4
		else 1
	in
	let rec loop acc i =
		if i >= len then List.rev acc
		else
			let n = min (width s.[i]) (len - i) in
			loop (String.sub s i n :: acc) (i + n)
	in
	loop [] 0

let ( let* ) = Result.bind
