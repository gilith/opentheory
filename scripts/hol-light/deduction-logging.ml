(* ========================================================================= *)
(* OpenTheory proof logging                                                  *)
(* Joe Hurd                                                                  *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Logging complex types.                                                    *)
(* ------------------------------------------------------------------------- *)

let log_justification f =
    log_function2 f log_inst (log_list log_thm) log_thm;;

let log_tactic t =
    log_function t log_error log_error o log_

let log_tactic =
    let rec log f tm =
        if is_const tm then
          let (n,ty) = dest_const tm in
            (log_name n; log_type ty; log_command "const")
        else if is_var tm then
          log_var tm
        else if is_comb tm then
          let (a,b) = dest_comb tm in
            (f a; f b; log_command "comb")
        else
          let (v,b) = dest_abs tm in
            (f v; f b; log_command "abs") in
    log_dict log;;


(* ------------------------------------------------------------------------- *)
(* Logged version of the higher level deductive system.                      *)
(* ------------------------------------------------------------------------- *)

let new_inductive_definition =
    log_function "new_inductive_definition"
      log_term (log_triple log_thm log_thm log_thm) new_inductive_definition;;

let new_definition =
    log_function "new_definition" log_term log_thm new_definition;;

let new_specification =
    log_function2 "new_specification"
      (log_list log_name) log_thm log_thm new_specification;;

let prove = log_function "prove" (log_map fst log_term) log_thm prove;;
