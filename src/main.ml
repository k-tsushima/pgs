open Test_minbigul
open Test_pg
open Test_xpg

let usage =
		"\nusage: .\\calc function_name comp_name n_comp \n" ^
		"\tfunction_name : { put | pg | xpg }\n" ^
		"\tcomp_name : { lassoc_comp_phead | rassoc_comp_phead | breverse }\n" ^
		"\tn_comp : positive number\n"

exception Missing_Parameter of string
exception Too_Many_Parameters of string
exception Unsupported_Parameter of string

let _ =
	match Array.length Sys.argv with
		| 1 | 2 | 3 ->
			raise (Missing_Parameter usage);
		| 4 ->
			let func_name = Sys.argv.(1) in
			let comp_name = Sys.argv.(2) in
			let n_comp = int_of_string (Sys.argv.(3)) in
				( match func_name, comp_name with
					| "put", "lassoc_comp_phead" -> let _ = put_lassoc_comp_phead n_comp in ""
					| "put", "rassoc_comp_phead" -> let _ = put_rassoc_comp_phead n_comp in ""
					| "put", "breverse" -> let _ = put_breverse n_comp in ""
					| "pg", "lassoc_comp_phead" -> let _ = pg_lassoc_comp_phead n_comp in ""
					| "pg", "rassoc_comp_phead" -> let _ = pg_rassoc_comp_phead n_comp in ""
					| "pg", "breverse" -> let _ = pg_breverse n_comp in ""
					| "xpg", "lassoc_comp_phead" -> let _ = xpg_lassoc_comp_phead n_comp in ""
					| "xpg", "rassoc_comp_phead" -> let _ = xpg_rassoc_comp_phead n_comp in ""
					| "xpg", "breverse" -> let _ = xpg_breverse n_comp in ""
					| _ -> raise (Unsupported_Parameter usage)
				)
		| _ ->
			raise (Too_Many_Parameters usage)