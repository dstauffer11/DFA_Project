module type REGEXPR = sig
	type string
	val accept_string : string -> bool
	val formula : () -> string
end