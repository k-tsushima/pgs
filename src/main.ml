open Test_minbigul
open Test_kpg
open Test_cpg
open Test_pg
open Test_xpg
open Gc

let usage =
		"\nusage: .\\calc function_name comp_name n_comp \n" ^
		"\tfunction_name : { put | pg | cpg | kpg | xpg }\n" ^
		"\tcomp_name : { lassoc_comp_phead | rassoc_comp_phead | breverse }\n" ^
		"\tn_comp : positive number\n"

exception Missing_Parameter of string
exception Too_Many_Parameters of string
exception Unsupported_Parameter of string

(* let _ = 
    let (p, g) = put_bmapreplace_count (int_of_string((Sys.argv.(1)))) in 
        Printf.printf "%d|%d" p g *)

(* let _ = 
    let pg = pg_bmapreplace_count (int_of_string((Sys.argv.(1)))) in 
        Printf.printf "%d" pg *)

let _ =
    let start_time = Sys.time () in 
	(match Array.length Sys.argv with
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
                    
                    | "kpg", "lassoc_comp_replace" -> let _ = kpg_lassoc_comp_replace n_comp in ""
					| "kpg", "rassoc_comp_replace" -> let _ = kpg_rassoc_comp_replace n_comp in ""
					| "kpg", "lassoc_comp_phead" -> let _ = kpg_lassoc_comp_phead n_comp in ""
					| "kpg", "rassoc_comp_phead" -> let _ = kpg_rassoc_comp_phead n_comp in ""
                    | "kpg", "lassoc_comp_phead2" -> let _ = kpg_lassoc_comp_phead2 n_comp in ""
					| "kpg", "rassoc_comp_phead2" -> let _ = kpg_rassoc_comp_phead2 n_comp in ""
                    | "kpg", "lassoc_comp_phead_with_case" -> let _ = kpg_lassoc_comp_phead_with_case n_comp in ""
					| "kpg", "rassoc_comp_phead_with_case" -> let _ = kpg_rassoc_comp_phead_with_case n_comp in ""
                    | "kpg", "lassoc_comp_phead2_with_case" -> let _ = kpg_lassoc_comp_phead2_with_case n_comp in ""
					| "kpg", "rassoc_comp_phead2_with_case" -> let _ = kpg_rassoc_comp_phead2_with_case n_comp in ""
					| "kpg", "breverse" -> let _ = kpg_breverse n_comp in ""
                    | "kpg", "bmapreplace" -> let _ = kpg_bmapreplace n_comp in ""
                    | "kpg", "bmapreplace_with_case" -> let _ = kpg_bmapreplace_with_case n_comp in ""

                    | "cpg", "lassoc_comp_replace" -> let _ = cpg_lassoc_comp_replace n_comp in ""
                    | "cpg", "rassoc_comp_replace" -> let _ = cpg_rassoc_comp_replace n_comp in ""
                    | "cpg", "lassoc_comp_phead" -> let _ = cpg_lassoc_comp_phead n_comp in ""
                    | "cpg", "rassoc_comp_phead" -> let _ = cpg_rassoc_comp_phead n_comp in ""
                    | "cpg", "lassoc_comp_phead2" -> let _ = cpg_lassoc_comp_phead2 n_comp in ""
                    | "cpg", "rassoc_comp_phead2" -> let _ = cpg_rassoc_comp_phead2 n_comp in ""
                    | "cpg", "lassoc_comp_phead_with_case" -> let _ = cpg_lassoc_comp_phead_with_case n_comp in ""
					| "cpg", "rassoc_comp_phead_with_case" -> let _ = cpg_rassoc_comp_phead_with_case n_comp in ""
                    | "cpg", "lassoc_comp_phead2_with_case" -> let _ = cpg_lassoc_comp_phead2_with_case n_comp in ""
					| "cpg", "rassoc_comp_phead2_with_case" -> let _ = cpg_rassoc_comp_phead2_with_case n_comp in ""
                    | "cpg", "breverse" -> let _ = cpg_breverse n_comp in ""
                    | "cpg", "bmapreplace" -> let _ = cpg_bmapreplace n_comp in ""
                    | "cpg", "bmapreplace_with_case" -> let _ = cpg_bmapreplace_with_case n_comp in ""

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

                    | "put", "lassoc_comp_phead_with_multi_case" -> let _ = put_lassoc_comp_phead_with_multi_case n_comp in ""
					| "put", "rassoc_comp_phead_with_multi_case" -> let _ = put_rassoc_comp_phead_with_multi_case n_comp in ""
                    | "xpg", "lassoc_comp_phead_with_multi_case" -> let _ = xpg_lassoc_comp_phead_with_multi_case n_comp in ""
					| "xpg", "rassoc_comp_phead_with_multi_case" -> let _ = xpg_rassoc_comp_phead_with_multi_case n_comp in ""

                    | "put", "lassoc_comp_phead_with_multi_case_2" -> let _ = put_lassoc_comp_phead_with_multi_case_2 n_comp in ""
					| "put", "rassoc_comp_phead_with_multi_case_2" -> let _ = put_rassoc_comp_phead_with_multi_case_2 n_comp in ""
                    | "xpg", "lassoc_comp_phead_with_multi_case_2" -> let _ = xpg_lassoc_comp_phead_with_multi_case_2 n_comp in ""
					| "xpg", "rassoc_comp_phead_with_multi_case_2" -> let _ = xpg_rassoc_comp_phead_with_multi_case_2 n_comp in ""

                    | "kpg", "lassoc_comp_phead_no_evaluation" -> let _ = kpg_lassoc_comp_phead_no_evaluation n_comp in ""
					| "kpg", "rassoc_comp_phead_no_evaluation" -> let _ = kpg_rassoc_comp_phead_no_evaluation n_comp in ""
                    
					| _ -> raise (Unsupported_Parameter usage)
				)
        | 1 | 2 | 3 ->
			raise (Missing_Parameter usage)
		| _ ->
			raise (Too_Many_Parameters usage)
        ); 
        Printf.printf "Elapsed Time: %f \nAllocated Bytes: %.0f \n" (Sys.time() -. start_time) (Gc.allocated_bytes ());