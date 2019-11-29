open Test_minbigul
open Test_minbigul_m
open Test_pg
open Test_cpg
open Test_kpg
open Test_xpg
open Gc

let usage = 
  "Usage: ./calc method fname ncomp\n" ^
  "  - method: put|put_m|pg|cpg|kpg|xpg\n" ^
  "  - fname:\n" ^ 
  "\t [l|r]assoc_comp_[replace|phead|phead_2|ptail|bsnoc](_ldata)?
  \t breverse(_ldata)?, bmapreplace(_ldata)?\n" ^
  "  - ncomp: number of compositions\n"

exception Missing_Parameter of string 
exception Too_Many_Parameters of string 
exception Unsupported_Parameter of string

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
       | "put", "lassoc_comp_ptail" -> let _ = put_lassoc_comp_ptail n_comp in ()
       | "put", "rassoc_comp_ptail" -> let _ = put_rassoc_comp_ptail n_comp in ()
       | "put", "lassoc_comp_phead_2" -> let _ = put_lassoc_comp_phead_2 n_comp in ()
       | "put", "rassoc_comp_phead_2" -> let _ = put_rassoc_comp_phead_2 n_comp in ()
       | "put", "lassoc_comp_phead2" -> let _ = put_lassoc_comp_phead2 n_comp in ()
       | "put", "rassoc_comp_phead2" -> let _ = put_rassoc_comp_phead2 n_comp in ()
       | "put", "breverse" -> let _ = put_breverse n_comp in ()
       | "put", "bmapreplace" -> let _ = put_bmapreplace n_comp in ()
       | "put", "lassoc_comp_bsnoc" -> let _ = put_lassoc_comp_bsnoc n_comp in ()
       | "put", "rassoc_comp_bsnoc" -> let _ = put_rassoc_comp_bsnoc n_comp in ()
       | "put", "lassoc_comp_bsnoc_ldata" -> let _ = put_lassoc_comp_bsnoc_ldata n_comp in ()
       | "put", "rassoc_comp_bsnoc_ldata" -> let _ = put_rassoc_comp_bsnoc_ldata n_comp in ()
       | "put", "lassoc_comp_ptail_ldata" -> let _ = put_lassoc_comp_ptail_ldata n_comp in ()
       | "put", "rassoc_comp_ptail_ldata" -> let _ = put_rassoc_comp_ptail_ldata n_comp in ()
       | "put", "lassoc_comp_replace_ldata" -> let _ = put_lassoc_comp_replace_ldata n_comp in ()
       | "put", "rassoc_comp_replace_ldata" -> let _ = put_rassoc_comp_replace_ldata n_comp in ()
       | "put", "breverse_ldata" -> let _ = put_breverse_ldata n_comp in ()
       | "put", "bmapreplace_ldata" -> let _ = put_bmapreplace_ldata n_comp in ()


       | "put_m", "lassoc_comp_replace" -> let _ = put_m_lassoc_comp_replace n_comp in ()
       | "put_m", "rassoc_comp_replace" -> let _ = put_m_rassoc_comp_replace n_comp in ()
       | "put_m", "lassoc_comp_phead" -> let _ = put_m_lassoc_comp_phead n_comp in ()
       | "put_m", "rassoc_comp_phead" -> let _ = put_m_rassoc_comp_phead n_comp in ()
       | "put_m", "lassoc_comp_ptail" -> let _ = put_m_lassoc_comp_ptail n_comp in ()
       | "put_m", "rassoc_comp_ptail" -> let _ = put_m_rassoc_comp_ptail n_comp in ()
       | "put_m", "lassoc_comp_phead_2" -> let _ = put_m_lassoc_comp_phead_2 n_comp in ()
       | "put_m", "rassoc_comp_phead_2" -> let _ = put_m_rassoc_comp_phead_2 n_comp in ()
       | "put_m", "lassoc_comp_phead2" -> let _ = put_m_lassoc_comp_phead2 n_comp in ()
       | "put_m", "rassoc_comp_phead2" -> let _ = put_m_rassoc_comp_phead2 n_comp in ()
       | "put_m", "breverse" -> let _ = put_m_breverse n_comp in ()
       | "put_m", "bmapreplace" -> let _ = put_m_bmapreplace n_comp in ()
       | "put_m", "lassoc_comp_bsnoc" -> let _ = put_m_lassoc_comp_bsnoc n_comp in ()
       | "put_m", "rassoc_comp_bsnoc" -> let _ = put_m_rassoc_comp_bsnoc n_comp in ()
       | "put_m", "lassoc_comp_bsnoc_ldata" -> let _ = put_m_lassoc_comp_bsnoc_ldata n_comp in ()
       | "put_m", "rassoc_comp_bsnoc_ldata" -> let _ = put_m_rassoc_comp_bsnoc_ldata n_comp in ()
       | "put_m", "lassoc_comp_ptail_ldata" -> let _ = put_m_lassoc_comp_ptail_ldata n_comp in ()
       | "put_m", "rassoc_comp_ptail_ldata" -> let _ = put_m_rassoc_comp_ptail_ldata n_comp in ()
       | "put_m", "lassoc_comp_replace_ldata" -> let _ = put_m_lassoc_comp_replace_ldata n_comp in ()
       | "put_m", "rassoc_comp_replace_ldata" -> let _ = put_m_rassoc_comp_replace_ldata n_comp in ()
       | "put_m", "breverse_ldata" -> let _ = put_m_breverse_ldata n_comp in ()
       | "put_m", "bmapreplace_ldata" -> let _ = put_m_bmapreplace_ldata n_comp in ()

       | "pg", "lassoc_comp_replace" -> let _ = pg_lassoc_comp_replace n_comp in ()
       | "pg", "rassoc_comp_replace" -> let _ = pg_rassoc_comp_replace n_comp in ()
       | "pg", "lassoc_comp_phead" -> let _ = pg_lassoc_comp_phead n_comp in ()
       | "pg", "rassoc_comp_phead" -> let _ = pg_rassoc_comp_phead n_comp in ()
       | "pg", "lassoc_comp_ptail" -> let _ = pg_lassoc_comp_ptail n_comp in ()
       | "pg", "rassoc_comp_ptail" -> let _ = pg_rassoc_comp_ptail n_comp in ()
       | "pg", "lassoc_comp_phead_2" -> let _ = pg_lassoc_comp_phead_2 n_comp in ()
       | "pg", "rassoc_comp_phead_2" -> let _ = pg_rassoc_comp_phead_2 n_comp in ()
       | "pg", "lassoc_comp_phead2" -> let _ = pg_lassoc_comp_phead2 n_comp in ()
       | "pg", "rassoc_comp_phead2" -> let _ = pg_rassoc_comp_phead2 n_comp in ()
       | "pg", "breverse" -> let _ = pg_breverse n_comp in ()
       | "pg", "bmapreplace" -> let _ = pg_bmapreplace n_comp in ()
       | "pg", "lassoc_comp_bsnoc" -> let _ = pg_lassoc_comp_bsnoc n_comp in ()
       | "pg", "rassoc_comp_bsnoc" -> let _ = pg_rassoc_comp_bsnoc n_comp in ()
       | "pg", "lassoc_comp_bsnoc_ldata" -> let _ = pg_lassoc_comp_bsnoc_ldata n_comp in ()
       | "pg", "rassoc_comp_bsnoc_ldata" -> let _ = pg_rassoc_comp_bsnoc_ldata n_comp in ()
       | "pg", "lassoc_comp_ptail_ldata" -> let _ = pg_lassoc_comp_ptail_ldata n_comp in ()
       | "pg", "rassoc_comp_ptail_ldata" -> let _ = pg_rassoc_comp_ptail_ldata n_comp in ()
       | "pg", "lassoc_comp_replace_ldata" -> let _ = pg_lassoc_comp_replace_ldata n_comp in ()
       | "pg", "rassoc_comp_replace_ldata" -> let _ = pg_rassoc_comp_replace_ldata n_comp in ()
       | "pg", "breverse_ldata" -> let _ = pg_breverse_ldata n_comp in ()
       | "pg", "bmapreplace_ldata" -> let _ = pg_bmapreplace_ldata n_comp in ()

       | "cpg", "lassoc_comp_replace" -> let _ = cpg_lassoc_comp_replace n_comp in ()
       | "cpg", "rassoc_comp_replace" -> let _ = cpg_rassoc_comp_replace n_comp in ()
       | "cpg", "lassoc_comp_phead" -> let _ = cpg_lassoc_comp_phead n_comp in ()
       | "cpg", "rassoc_comp_phead" -> let _ = cpg_rassoc_comp_phead n_comp in ()
       | "cpg", "lassoc_comp_ptail" -> let _ = cpg_lassoc_comp_ptail n_comp in ()
       | "cpg", "rassoc_comp_ptail" -> let _ = cpg_rassoc_comp_ptail n_comp in ()
       | "cpg", "lassoc_comp_phead_2" -> let _ = cpg_lassoc_comp_phead_2 n_comp in ()
       | "cpg", "rassoc_comp_phead_2" -> let _ = cpg_rassoc_comp_phead_2 n_comp in ()
       | "cpg", "lassoc_comp_phead2" -> let _ = cpg_lassoc_comp_phead2 n_comp in ()
       | "cpg", "rassoc_comp_phead2" -> let _ = cpg_rassoc_comp_phead2 n_comp in ()
       | "cpg", "breverse" -> let _ = cpg_breverse n_comp in ()
       | "cpg", "bmapreplace" -> let _ = cpg_bmapreplace n_comp in ()
       | "cpg", "lassoc_comp_bsnoc" -> let _ = cpg_lassoc_comp_bsnoc n_comp in ()
       | "cpg", "rassoc_comp_bsnoc" -> let _ = cpg_rassoc_comp_bsnoc n_comp in ()
       | "cpg", "lassoc_comp_bsnoc_ldata" -> let _ = cpg_lassoc_comp_bsnoc_ldata n_comp in ()
       | "cpg", "rassoc_comp_bsnoc_ldata" -> let _ = cpg_rassoc_comp_bsnoc_ldata n_comp in ()
       | "cpg", "lassoc_comp_ptail_ldata" -> let _ = cpg_lassoc_comp_ptail_ldata n_comp in ()
       | "cpg", "rassoc_comp_ptail_ldata" -> let _ = cpg_rassoc_comp_ptail_ldata n_comp in ()
       | "cpg", "lassoc_comp_replace_ldata" -> let _ = cpg_lassoc_comp_replace_ldata n_comp in ()
       | "cpg", "rassoc_comp_replace_ldata" -> let _ = cpg_rassoc_comp_replace_ldata n_comp in ()
       | "cpg", "breverse_ldata" -> let _ = cpg_breverse_ldata n_comp in ()
       | "cpg", "bmapreplace_ldata" -> let _ = cpg_bmapreplace_ldata n_comp in ()

       | "kpg", "lassoc_comp_replace" -> let _ = kpg_lassoc_comp_replace n_comp in ()
       | "kpg", "rassoc_comp_replace" -> let _ = kpg_rassoc_comp_replace n_comp in ()
       | "kpg", "lassoc_comp_phead" -> let _ = kpg_lassoc_comp_phead n_comp in ()
       | "kpg", "rassoc_comp_phead" -> let _ = kpg_rassoc_comp_phead n_comp in ()
       | "kpg", "lassoc_comp_ptail" -> let _ = kpg_lassoc_comp_ptail n_comp in ()
       | "kpg", "rassoc_comp_ptail" -> let _ = kpg_rassoc_comp_ptail n_comp in ()
       | "kpg", "lassoc_comp_phead_2" -> let _ = kpg_lassoc_comp_phead_2 n_comp in ()
       | "kpg", "rassoc_comp_phead_2" -> let _ = kpg_rassoc_comp_phead_2 n_comp in ()
       | "kpg", "lassoc_comp_phead2" -> let _ = kpg_lassoc_comp_phead2 n_comp in ()
       | "kpg", "rassoc_comp_phead2" -> let _ = kpg_rassoc_comp_phead2 n_comp in ()
       | "kpg", "breverse" -> let _ = kpg_breverse n_comp in ()
       | "kpg", "bmapreplace" -> let _ = kpg_bmapreplace n_comp in ()
       | "kpg", "lassoc_comp_bsnoc" -> let _ = kpg_lassoc_comp_bsnoc n_comp in ()
       | "kpg", "rassoc_comp_bsnoc" -> let _ = kpg_rassoc_comp_bsnoc n_comp in ()
       | "kpg", "lassoc_comp_bsnoc_ldata" -> let _ = kpg_lassoc_comp_bsnoc_ldata n_comp in ()
       | "kpg", "rassoc_comp_bsnoc_ldata" -> let _ = kpg_rassoc_comp_bsnoc_ldata n_comp in ()
       | "kpg", "lassoc_comp_ptail_ldata" -> let _ = kpg_lassoc_comp_ptail_ldata n_comp in ()
       | "kpg", "rassoc_comp_ptail_ldata" -> let _ = kpg_rassoc_comp_ptail_ldata n_comp in ()
       | "kpg", "lassoc_comp_replace_ldata" -> let _ = kpg_lassoc_comp_replace_ldata n_comp in ()
       | "kpg", "rassoc_comp_replace_ldata" -> let _ = kpg_rassoc_comp_replace_ldata n_comp in ()
       | "kpg", "breverse_ldata" -> let _ = kpg_breverse_ldata n_comp in ()
       | "kpg", "bmapreplace_ldata" -> let _ = kpg_bmapreplace_ldata n_comp in ()

       | "xpg", "lassoc_comp_replace" -> let _ = xpg_lassoc_comp_replace n_comp in ()
       | "xpg", "rassoc_comp_replace" -> let _ = xpg_rassoc_comp_replace n_comp in ()
       | "xpg", "lassoc_comp_phead" -> let _ = xpg_lassoc_comp_phead n_comp in ()
       | "xpg", "rassoc_comp_phead" -> let _ = xpg_rassoc_comp_phead n_comp in ()
       | "xpg", "lassoc_comp_ptail" -> let _ = xpg_lassoc_comp_ptail n_comp in ()
       | "xpg", "rassoc_comp_ptail" -> let _ = xpg_rassoc_comp_ptail n_comp in ()
       | "xpg", "lassoc_comp_phead_2" -> let _ = xpg_lassoc_comp_phead_2 n_comp in ()
       | "xpg", "rassoc_comp_phead_2" -> let _ = xpg_rassoc_comp_phead_2 n_comp in ()
       | "xpg", "lassoc_comp_phead2" -> let _ = xpg_lassoc_comp_phead2 n_comp in ()
       | "xpg", "rassoc_comp_phead2" -> let _ = xpg_rassoc_comp_phead2 n_comp in ()
       | "xpg", "breverse" -> let _ = xpg_breverse n_comp in ()
       | "xpg", "bmapreplace" -> let _ = xpg_bmapreplace n_comp in ()
       | "xpg", "lassoc_comp_bsnoc" -> let _ = xpg_lassoc_comp_bsnoc n_comp in ()
       | "xpg", "rassoc_comp_bsnoc" -> let _ = xpg_rassoc_comp_bsnoc n_comp in ()
       | "xpg", "lassoc_comp_bsnoc_ldata" -> let _ = xpg_lassoc_comp_bsnoc_ldata n_comp in ()
       | "xpg", "rassoc_comp_bsnoc_ldata" -> let _ = xpg_rassoc_comp_bsnoc_ldata n_comp in ()
       | "xpg", "lassoc_comp_ptail_ldata" -> let _ = xpg_lassoc_comp_ptail_ldata n_comp in ()
       | "xpg", "rassoc_comp_ptail_ldata" -> let _ = xpg_rassoc_comp_ptail_ldata n_comp in ()
       | "xpg", "lassoc_comp_replace_ldata" -> let _ = xpg_lassoc_comp_replace_ldata n_comp in ()
       | "xpg", "rassoc_comp_replace_ldata" -> let _ = xpg_rassoc_comp_replace_ldata n_comp in ()
       | "xpg", "breverse_ldata" -> let _ = xpg_breverse_ldata n_comp in ()
       | "xpg", "bmapreplace_ldata" -> let _ = xpg_bmapreplace_ldata n_comp in ()

       | _ -> raise (Unsupported_Parameter ("\n" ^ usage))
     )
   | 1 | 2 | 3 ->
     raise (Missing_Parameter ("\n" ^ usage))
   | _ ->
     raise (Too_Many_Parameters ("\n" ^ usage))
  ); 
  Printf.printf "Elapsed Time: %f \nAllocated Bytes: %.0f \n" (Sys.time() -. start_time) (Gc.allocated_bytes ());