open Test_minbigul
open Test_minbigul_m
open Test_pg
open Test_cpg
open Test_kpg
open Test_cpg2
open Test_kpg2
open Gc

exception Missing_Parameter
exception Too_Many_Parameters
exception Unsupported_Parameter

let _ =
  let start_time = Sys.time () in 
  (match Array.length Sys.argv with
   | 4 ->
     let func_name = Sys.argv.(1) in
     let comp_name = Sys.argv.(2) in
     let n_comp = int_of_string (Sys.argv.(3)) in
     ( match func_name, comp_name with
       | "put", "lassoc_comp_replace" -> let _ = put_lassoc_comp_replace n_comp in ()
       | "put", "rassoc_comp_replace" -> let _ = put_rassoc_comp_replace n_comp in ()
       | "put", "lassoc_comp_phead" -> let _ = put_lassoc_comp_phead n_comp in ()
       | "put", "rassoc_comp_phead" -> let _ = put_rassoc_comp_phead n_comp in ()
       | "put", "lassoc_comp_phead2" -> let _ = put_lassoc_comp_phead2 n_comp in ()
       | "put", "rassoc_comp_phead2" -> let _ = put_rassoc_comp_phead2 n_comp in ()
       | "put", "breverse" -> let _ = put_breverse n_comp in ()
       | "put", "lassoc_comp_breverse" -> let _ = put_lassoc_comp_breverse n_comp in ()
       | "put", "rassoc_comp_breverse" -> let _ = put_rassoc_comp_breverse n_comp in ()
       | "put", "bmapreplace" -> let _ = put_bmapreplace n_comp in ()
       | "put", "lassoc_comp_bsnoc" -> let _ = put_lassoc_comp_bsnoc n_comp in ()
       | "put", "rassoc_comp_bsnoc" -> let _ = put_rassoc_comp_bsnoc n_comp in ()
       | "put", "lassoc_comp_bsnoc_nested_list" -> let _ = put_lassoc_comp_bsnoc_nested_list n_comp in ()
       | "put", "rassoc_comp_bsnoc_nested_list" -> let _ = put_rassoc_comp_bsnoc_nested_list n_comp in ()
       | "put", "lassoc_comp_replace_nested_list" -> let _ = put_lassoc_comp_replace_nested_list n_comp in ()
       | "put", "rassoc_comp_replace_nested_list" -> let _ = put_rassoc_comp_replace_nested_list n_comp in ()
       | "put", "breverse_nested_list" -> let _ = put_breverse_nested_list n_comp in ()
       | "put", "bmapreplace_nested_list" -> let _ = put_bmapreplace_nested_list n_comp in ()

       | "put_m", "lassoc_comp_replace" -> let _ = put_m_lassoc_comp_replace n_comp in ()
       | "put_m", "rassoc_comp_replace" -> let _ = put_m_rassoc_comp_replace n_comp in ()
       | "put_m", "lassoc_comp_phead" -> let _ = put_m_lassoc_comp_phead n_comp in ()
       | "put_m", "rassoc_comp_phead" -> let _ = put_m_rassoc_comp_phead n_comp in ()
       | "put_m", "lassoc_comp_phead2" -> let _ = put_m_lassoc_comp_phead2 n_comp in ()
       | "put_m", "rassoc_comp_phead2" -> let _ = put_m_rassoc_comp_phead2 n_comp in ()
       | "put_m", "breverse" -> let _ = put_m_breverse n_comp in ()
       | "put_m", "lassoc_comp_breverse" -> let _ = put_m_lassoc_comp_breverse n_comp in ()
       | "put_m", "rassoc_comp_breverse" -> let _ = put_m_rassoc_comp_breverse n_comp in ()
       | "put_m", "bmapreplace" -> let _ = put_m_bmapreplace n_comp in ()
       | "put_m", "lassoc_comp_bsnoc" -> let _ = put_m_lassoc_comp_bsnoc n_comp in ()
       | "put_m", "rassoc_comp_bsnoc" -> let _ = put_m_rassoc_comp_bsnoc n_comp in ()
       | "put_m", "lassoc_comp_bsnoc_nested_list" -> let _ = put_m_lassoc_comp_bsnoc_nested_list n_comp in ()
       | "put_m", "rassoc_comp_bsnoc_nested_list" -> let _ = put_m_rassoc_comp_bsnoc_nested_list n_comp in ()
       | "put_m", "lassoc_comp_replace_nested_list" -> let _ = put_m_lassoc_comp_replace_nested_list n_comp in ()
       | "put_m", "rassoc_comp_replace_nested_list" -> let _ = put_m_rassoc_comp_replace_nested_list n_comp in ()
       | "put_m", "breverse_nested_list" -> let _ = put_m_breverse_nested_list n_comp in ()
       | "put_m", "bmapreplace_nested_list" -> let _ = put_m_bmapreplace_nested_list n_comp in ()

       | "pg", "lassoc_comp_replace" -> let _ = pg_lassoc_comp_replace n_comp in ()
       | "pg", "rassoc_comp_replace" -> let _ = pg_rassoc_comp_replace n_comp in ()
       | "pg", "lassoc_comp_phead" -> let _ = pg_lassoc_comp_phead n_comp in ()
       | "pg", "rassoc_comp_phead" -> let _ = pg_rassoc_comp_phead n_comp in ()
       | "pg", "lassoc_comp_phead2" -> let _ = pg_lassoc_comp_phead2 n_comp in ()
       | "pg", "rassoc_comp_phead2" -> let _ = pg_rassoc_comp_phead2 n_comp in ()
       | "pg", "breverse" -> let _ = pg_breverse n_comp in ()
       | "pg", "lassoc_comp_breverse" -> let _ = pg_lassoc_comp_breverse n_comp in ()
       | "pg", "rassoc_comp_breverse" -> let _ = pg_rassoc_comp_breverse n_comp in ()
       | "pg", "bmapreplace" -> let _ = pg_bmapreplace n_comp in ()
       | "pg", "lassoc_comp_bsnoc" -> let _ = pg_lassoc_comp_bsnoc n_comp in ()
       | "pg", "rassoc_comp_bsnoc" -> let _ = pg_rassoc_comp_bsnoc n_comp in ()
       | "pg", "lassoc_comp_bsnoc_nested_list" -> let _ = pg_lassoc_comp_bsnoc_nested_list n_comp in ()
       | "pg", "rassoc_comp_bsnoc_nested_list" -> let _ = pg_rassoc_comp_bsnoc_nested_list n_comp in ()
       | "pg", "lassoc_comp_replace_nested_list" -> let _ = pg_lassoc_comp_replace_nested_list n_comp in ()
       | "pg", "rassoc_comp_replace_nested_list" -> let _ = pg_rassoc_comp_replace_nested_list n_comp in ()
       | "pg", "breverse_nested_list" -> let _ = pg_breverse_nested_list n_comp in ()
       | "pg", "bmapreplace_nested_list" -> let _ = pg_bmapreplace_nested_list n_comp in ()

       | "cpg", "lassoc_comp_replace" -> let _ = cpg_lassoc_comp_replace n_comp in ()
       | "cpg", "rassoc_comp_replace" -> let _ = cpg_rassoc_comp_replace n_comp in ()
       | "cpg", "lassoc_comp_phead" -> let _ = cpg_lassoc_comp_phead n_comp in ()
       | "cpg", "rassoc_comp_phead" -> let _ = cpg_rassoc_comp_phead n_comp in ()
       | "cpg", "lassoc_comp_phead2" -> let _ = cpg_lassoc_comp_phead2 n_comp in ()
       | "cpg", "rassoc_comp_phead2" -> let _ = cpg_rassoc_comp_phead2 n_comp in ()
       | "cpg", "breverse" -> let _ = cpg_breverse n_comp in ()
       | "cpg", "lassoc_comp_breverse" -> let _ = cpg_lassoc_comp_breverse n_comp in ()
       | "cpg", "rassoc_comp_breverse" -> let _ = cpg_rassoc_comp_breverse n_comp in ()
       | "cpg", "bmapreplace" -> let _ = cpg_bmapreplace n_comp in ()
       | "cpg", "lassoc_comp_bsnoc" -> let _ = cpg_lassoc_comp_bsnoc n_comp in ()
       | "cpg", "rassoc_comp_bsnoc" -> let _ = cpg_rassoc_comp_bsnoc n_comp in ()
       | "cpg", "lassoc_comp_bsnoc_nested_list" -> let _ = cpg_lassoc_comp_bsnoc_nested_list n_comp in ()
       | "cpg", "rassoc_comp_bsnoc_nested_list" -> let _ = cpg_rassoc_comp_bsnoc_nested_list n_comp in ()
       | "cpg", "lassoc_comp_replace_nested_list" -> let _ = cpg_lassoc_comp_replace_nested_list n_comp in ()
       | "cpg", "rassoc_comp_replace_nested_list" -> let _ = cpg_rassoc_comp_replace_nested_list n_comp in ()
       | "cpg", "breverse_nested_list" -> let _ = cpg_breverse_nested_list n_comp in ()
       | "cpg", "bmapreplace_nested_list" -> let _ = cpg_bmapreplace_nested_list n_comp in ()

       | "cpg2", "lassoc_comp_replace" -> let _ = cpg2_lassoc_comp_replace n_comp in ()
       | "cpg2", "rassoc_comp_replace" -> let _ = cpg2_rassoc_comp_replace n_comp in ()
       | "cpg2", "lassoc_comp_phead" -> let _ = cpg2_lassoc_comp_phead n_comp in ()
       | "cpg2", "rassoc_comp_phead" -> let _ = cpg2_rassoc_comp_phead n_comp in ()
       | "cpg2", "lassoc_comp_phead2" -> let _ = cpg2_lassoc_comp_phead2 n_comp in ()
       | "cpg2", "rassoc_comp_phead2" -> let _ = cpg2_rassoc_comp_phead2 n_comp in ()
       | "cpg2", "breverse" -> let _ = cpg2_breverse n_comp in ()
       | "cpg2", "lassoc_comp_breverse" -> let _ = cpg2_lassoc_comp_breverse n_comp in ()
       | "cpg2", "rassoc_comp_breverse" -> let _ = cpg2_rassoc_comp_breverse n_comp in ()
       | "cpg2", "bmapreplace" -> let _ = cpg2_bmapreplace n_comp in ()
       | "cpg2", "lassoc_comp_bsnoc" -> let _ = cpg2_lassoc_comp_bsnoc n_comp in ()
       | "cpg2", "rassoc_comp_bsnoc" -> let _ = cpg2_rassoc_comp_bsnoc n_comp in ()
       | "cpg2", "lassoc_comp_bsnoc_nested_list" -> let _ = cpg2_lassoc_comp_bsnoc_nested_list n_comp in ()
       | "cpg2", "rassoc_comp_bsnoc_nested_list" -> let _ = cpg2_rassoc_comp_bsnoc_nested_list n_comp in ()
       | "cpg2", "lassoc_comp_replace_nested_list" -> let _ = cpg2_lassoc_comp_replace_nested_list n_comp in ()
       | "cpg2", "rassoc_comp_replace_nested_list" -> let _ = cpg2_rassoc_comp_replace_nested_list n_comp in ()
       | "cpg2", "breverse_nested_list" -> let _ = cpg2_breverse_nested_list n_comp in ()
       | "cpg2", "bmapreplace_nested_list" -> let _ = cpg2_bmapreplace_nested_list n_comp in ()

       | "kpg", "lassoc_comp_replace" -> let _ = kpg_lassoc_comp_replace n_comp in ()
       | "kpg", "rassoc_comp_replace" -> let _ = kpg_rassoc_comp_replace n_comp in ()
       | "kpg", "lassoc_comp_phead" -> let _ = kpg_lassoc_comp_phead n_comp in ()
       | "kpg", "rassoc_comp_phead" -> let _ = kpg_rassoc_comp_phead n_comp in ()
       | "kpg", "lassoc_comp_phead2" -> let _ = kpg_lassoc_comp_phead2 n_comp in ()
       | "kpg", "rassoc_comp_phead2" -> let _ = kpg_rassoc_comp_phead2 n_comp in ()
       | "kpg", "breverse" -> let _ = kpg_breverse n_comp in ()
       | "kpg", "lassoc_comp_breverse" -> let _ = kpg_lassoc_comp_breverse n_comp in ()
       | "kpg", "rassoc_comp_breverse" -> let _ = kpg_rassoc_comp_breverse n_comp in ()
       | "kpg", "bmapreplace" -> let _ = kpg_bmapreplace n_comp in ()
       | "kpg", "lassoc_comp_bsnoc" -> let _ = kpg_lassoc_comp_bsnoc n_comp in ()
       | "kpg", "rassoc_comp_bsnoc" -> let _ = kpg_rassoc_comp_bsnoc n_comp in ()
       | "kpg", "lassoc_comp_bsnoc_nested_list" -> let _ = kpg_lassoc_comp_bsnoc_nested_list n_comp in ()
       | "kpg", "rassoc_comp_bsnoc_nested_list" -> let _ = kpg_rassoc_comp_bsnoc_nested_list n_comp in ()
       | "kpg", "lassoc_comp_replace_nested_list" -> let _ = kpg_lassoc_comp_replace_nested_list n_comp in ()
       | "kpg", "rassoc_comp_replace_nested_list" -> let _ = kpg_rassoc_comp_replace_nested_list n_comp in ()
       | "kpg", "breverse_nested_list" -> let _ = kpg_breverse_nested_list n_comp in ()
       | "kpg", "bmapreplace_nested_list" -> let _ = kpg_bmapreplace_nested_list n_comp in ()

       | "kpg2", "lassoc_comp_replace" -> let _ = kpg2_lassoc_comp_replace n_comp in ()
       | "kpg2", "rassoc_comp_replace" -> let _ = kpg2_rassoc_comp_replace n_comp in ()
       | "kpg2", "lassoc_comp_phead" -> let _ = kpg2_lassoc_comp_phead n_comp in ()
       | "kpg2", "rassoc_comp_phead" -> let _ = kpg2_rassoc_comp_phead n_comp in ()
       | "kpg2", "lassoc_comp_phead2" -> let _ = kpg2_lassoc_comp_phead2 n_comp in ()
       | "kpg2", "rassoc_comp_phead2" -> let _ = kpg2_rassoc_comp_phead2 n_comp in ()
       | "kpg2", "breverse" -> let _ = kpg2_breverse n_comp in ()
       | "kpg2", "lassoc_comp_breverse" -> let _ = kpg2_lassoc_comp_breverse n_comp in ()
       | "kpg2", "rassoc_comp_breverse" -> let _ = kpg2_rassoc_comp_breverse n_comp in ()
       | "kpg2", "bmapreplace" -> let _ = kpg2_bmapreplace n_comp in ()
       | "kpg2", "lassoc_comp_bsnoc" -> let _ = kpg2_lassoc_comp_bsnoc n_comp in ()
       | "kpg2", "rassoc_comp_bsnoc" -> let _ = kpg2_rassoc_comp_bsnoc n_comp in ()
       | "kpg2", "lassoc_comp_bsnoc_nested_list" -> let _ = kpg2_lassoc_comp_bsnoc_nested_list n_comp in ()
       | "kpg2", "rassoc_comp_bsnoc_nested_list" -> let _ = kpg2_rassoc_comp_bsnoc_nested_list n_comp in ()
       | "kpg2", "lassoc_comp_replace_nested_list" -> let _ = kpg2_lassoc_comp_replace_nested_list n_comp in ()
       | "kpg2", "rassoc_comp_replace_nested_list" -> let _ = kpg2_rassoc_comp_replace_nested_list n_comp in ()
       | "kpg2", "breverse_nested_list" -> let _ = kpg2_breverse_nested_list n_comp in ()
       | "kpg2", "bmapreplace_nested_list" -> let _ = kpg2_bmapreplace_nested_list n_comp in ()

       | "put", "lassoc_comp_ptail" -> let _ = put_lassoc_comp_ptail n_comp in ()
       | "put", "rassoc_comp_ptail" -> let _ = put_rassoc_comp_ptail n_comp in ()
       | "put", "lassoc_comp_ptail_nested_list" -> let _ = put_lassoc_comp_ptail_nested_list n_comp in ()
       | "put", "rassoc_comp_ptail_nested_list" -> let _ = put_rassoc_comp_ptail_nested_list n_comp in ()
       | "put", "lassoc_comp_phead_2" -> let _ = put_lassoc_comp_phead_2 n_comp in ()
       | "put", "rassoc_comp_phead_2" -> let _ = put_rassoc_comp_phead_2 n_comp in ()
       | "put", "rassoc_comp_breverse_nested_list" -> let _ = put_rassoc_comp_breverse_nested_list n_comp in ()

       | "put_m", "lassoc_comp_ptail" -> let _ = put_m_lassoc_comp_ptail n_comp in ()
       | "put_m", "rassoc_comp_ptail" -> let _ = put_m_rassoc_comp_ptail n_comp in ()
       | "put_m", "lassoc_comp_ptail_nested_list" -> let _ = put_m_lassoc_comp_ptail_nested_list n_comp in ()
       | "put_m", "rassoc_comp_ptail_nested_list" -> let _ = put_m_rassoc_comp_ptail_nested_list n_comp in ()
       | "put_m", "lassoc_comp_phead_2" -> let _ = put_m_lassoc_comp_phead_2 n_comp in ()
       | "put_m", "rassoc_comp_phead_2" -> let _ = put_m_rassoc_comp_phead_2 n_comp in ()
       | "put_m", "rassoc_comp_breverse_nested_list" -> let _ = put_m_rassoc_comp_breverse_nested_list n_comp in ()

       | "pg", "lassoc_comp_ptail" -> let _ = pg_lassoc_comp_ptail n_comp in ()
       | "pg", "rassoc_comp_ptail" -> let _ = pg_rassoc_comp_ptail n_comp in ()
       | "pg", "lassoc_comp_phead_2" -> let _ = pg_lassoc_comp_phead_2 n_comp in ()
       | "pg", "rassoc_comp_phead_2" -> let _ = pg_rassoc_comp_phead_2 n_comp in ()

       | "kpg2", "lassoc_comp_ptail" -> let _ = kpg2_lassoc_comp_ptail n_comp in ()
       | "kpg2", "rassoc_comp_ptail" -> let _ = kpg2_rassoc_comp_ptail n_comp in ()
       | "kpg2", "lassoc_comp_ptail_nested_list" -> let _ = kpg2_lassoc_comp_ptail_nested_list n_comp in ()
       | "kpg2", "rassoc_comp_ptail_nested_list" -> let _ = kpg2_rassoc_comp_ptail_nested_list n_comp in ()
       | "kpg2", "lassoc_comp_phead_2" -> let _ = kpg2_lassoc_comp_phead_2 n_comp in ()
       | "kpg2", "rassoc_comp_phead_2" -> let _ = kpg2_rassoc_comp_phead_2 n_comp in ()
       | "kpg2", "rassoc_comp_breverse_nested_list" -> let _ = kpg2_rassoc_comp_breverse_nested_list n_comp in ()
       | _ -> raise Unsupported_Parameter
     )
   | 1 | 2 | 3 ->
     raise Missing_Parameter
   | _ ->
     raise Too_Many_Parameters
  ); 
  Printf.printf "Elapsed Time: %f \nAllocated Bytes: %.0f \n" (Sys.time() -. start_time) (Gc.allocated_bytes ());