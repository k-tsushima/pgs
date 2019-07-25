open Test_minbigul
open Test_xpg
open Test_xpg2
open Test_pg
open Test_pg2

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
                    | "put", "lassoc_comp_replace" -> let _ = put_lassoc_comp_replace n_comp in ""
					| "put", "rassoc_comp_replace" -> let _ = put_rassoc_comp_replace n_comp in ""
					| "put", "lassoc_comp_phead" -> let _ = put_lassoc_comp_phead n_comp in ""
					| "put", "rassoc_comp_phead" -> let _ = put_rassoc_comp_phead n_comp in ""
                    | "put", "lassoc_comp_phead2" -> let _ = put_lassoc_comp_phead2 n_comp in ""
					| "put", "rassoc_comp_phead2" -> let _ = put_rassoc_comp_phead2 n_comp in ""
                    | "put", "lassoc_comp_phead_with_case" -> let _ = put_lassoc_comp_phead_with_case n_comp in ""
					| "put", "rassoc_comp_phead_with_case" -> let _ = put_rassoc_comp_phead_with_case n_comp in ""
                    | "put", "lassoc_comp_phead2_with_case" -> let _ = put_lassoc_comp_phead2_with_case n_comp in ""
					| "put", "rassoc_comp_phead2_with_case" -> let _ = put_rassoc_comp_phead2_with_case n_comp in ""
					| "put", "breverse" -> let _ = put_breverse n_comp in ""
                    | "put", "bmapreplace" -> let _ = put_bmapreplace n_comp in ""
                    | "put", "bmapreplace_with_case" -> let _ = put_bmapreplace_with_case n_comp in ""
                    
                    | "xpg", "lassoc_comp_replace" -> let _ = xpg_lassoc_comp_replace n_comp in ""
					| "xpg", "rassoc_comp_replace" -> let _ = xpg_rassoc_comp_replace n_comp in ""
					| "xpg", "lassoc_comp_phead" -> let _ = xpg_lassoc_comp_phead n_comp in ""
					| "xpg", "rassoc_comp_phead" -> let _ = xpg_rassoc_comp_phead n_comp in ""
                    | "xpg", "lassoc_comp_phead2" -> let _ = xpg_lassoc_comp_phead2 n_comp in ""
					| "xpg", "rassoc_comp_phead2" -> let _ = xpg_rassoc_comp_phead2 n_comp in ""
                    | "xpg", "lassoc_comp_phead_with_case" -> let _ = xpg_lassoc_comp_phead_with_case n_comp in ""
					| "xpg", "rassoc_comp_phead_with_case" -> let _ = xpg_rassoc_comp_phead_with_case n_comp in ""
                    | "xpg", "lassoc_comp_phead2_with_case" -> let _ = xpg_lassoc_comp_phead2_with_case n_comp in ""
					| "xpg", "rassoc_comp_phead2_with_case" -> let _ = xpg_rassoc_comp_phead2_with_case n_comp in ""
					| "xpg", "breverse" -> let _ = xpg_breverse n_comp in ""
                    | "xpg", "bmapreplace" -> let _ = xpg_bmapreplace n_comp in ""
                    | "xpg", "bmapreplace_with_case" -> let _ = xpg_bmapreplace_with_case n_comp in ""

                    | "xpg2", "lassoc_comp_replace" -> let _ = xpg2_lassoc_comp_replace n_comp in ""
                    | "xpg2", "rassoc_comp_replace" -> let _ = xpg2_rassoc_comp_replace n_comp in ""
                    | "xpg2", "lassoc_comp_phead" -> let _ = xpg2_lassoc_comp_phead n_comp in ""
                    | "xpg2", "rassoc_comp_phead" -> let _ = xpg2_rassoc_comp_phead n_comp in ""
                    | "xpg2", "lassoc_comp_phead2" -> let _ = xpg2_lassoc_comp_phead2 n_comp in ""
                    | "xpg2", "rassoc_comp_phead2" -> let _ = xpg2_rassoc_comp_phead2 n_comp in ""
                    | "xpg2", "lassoc_comp_phead_with_case" -> let _ = xpg2_lassoc_comp_phead_with_case n_comp in ""
					| "xpg2", "rassoc_comp_phead_with_case" -> let _ = xpg2_rassoc_comp_phead_with_case n_comp in ""
                    | "xpg2", "lassoc_comp_phead2_with_case" -> let _ = xpg2_lassoc_comp_phead2_with_case n_comp in ""
					| "xpg2", "rassoc_comp_phead2_with_case" -> let _ = xpg2_rassoc_comp_phead2_with_case n_comp in ""
                    | "xpg2", "breverse" -> let _ = xpg2_breverse n_comp in ""
                    | "xpg2", "bmapreplace" -> let _ = xpg2_bmapreplace n_comp in ""
                    | "xpg2", "bmapreplace_with_case" -> let _ = xpg2_bmapreplace_with_case n_comp in ""

                    | "pg", "lassoc_comp_replace" -> let _ = pg_lassoc_comp_replace n_comp in ""
					| "pg", "rassoc_comp_replace" -> let _ = pg_rassoc_comp_replace n_comp in ""
					| "pg", "lassoc_comp_phead" -> let _ = pg_lassoc_comp_phead n_comp in ""
					| "pg", "rassoc_comp_phead" -> let _ = pg_rassoc_comp_phead n_comp in ""
                    | "pg", "lassoc_comp_phead2" -> let _ = pg_lassoc_comp_phead2 n_comp in ""
					| "pg", "rassoc_comp_phead2" -> let _ = pg_rassoc_comp_phead2 n_comp in ""
                    | "pg", "lassoc_comp_phead_with_case" -> let _ = pg_lassoc_comp_phead_with_case n_comp in ""
					| "pg", "rassoc_comp_phead_with_case" -> let _ = pg_rassoc_comp_phead_with_case n_comp in ""
                    | "pg", "lassoc_comp_phead2_with_case" -> let _ = pg_lassoc_comp_phead2_with_case n_comp in ""
					| "pg", "rassoc_comp_phead2_with_case" -> let _ = pg_rassoc_comp_phead2_with_case n_comp in ""
					| "pg", "breverse" -> let _ = pg_breverse n_comp in ""
                    | "pg", "bmapreplace" -> let _ = pg_bmapreplace n_comp in ""
                    | "pg", "bmapreplace_with_case" -> let _ = pg_bmapreplace_with_case n_comp in ""

                    | "pg2", "lassoc_comp_replace" -> let _ = pg2_lassoc_comp_replace n_comp in ""
					| "pg2", "rassoc_comp_replace" -> let _ = pg2_rassoc_comp_replace n_comp in ""
					| "pg2", "lassoc_comp_phead" -> let _ = pg2_lassoc_comp_phead n_comp in ""
					| "pg2", "rassoc_comp_phead" -> let _ = pg2_rassoc_comp_phead n_comp in ""
                    | "pg2", "lassoc_comp_phead2" -> let _ = pg2_lassoc_comp_phead2 n_comp in ""
					| "pg2", "rassoc_comp_phead2" -> let _ = pg2_rassoc_comp_phead2 n_comp in ""
                    | "pg2", "lassoc_comp_phead_with_case" -> let _ = pg2_lassoc_comp_phead_with_case n_comp in ""
					| "pg2", "rassoc_comp_phead_with_case" -> let _ = pg2_rassoc_comp_phead_with_case n_comp in ""
                    | "pg2", "lassoc_comp_phead2_with_case" -> let _ = pg2_lassoc_comp_phead2_with_case n_comp in ""
					| "pg2", "rassoc_comp_phead2_with_case" -> let _ = pg2_rassoc_comp_phead2_with_case n_comp in ""
					| "pg2", "breverse" -> let _ = pg2_breverse n_comp in ""
                    | "pg2", "bmapreplace" -> let _ = pg2_bmapreplace n_comp in ""
                    | "pg2", "bmapreplace_with_case" -> let _ = pg2_bmapreplace_with_case n_comp in ""
                    
					| _ -> raise (Unsupported_Parameter usage)
				)
		| _ ->
			raise (Too_Many_Parameters usage)