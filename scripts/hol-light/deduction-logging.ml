(* ========================================================================= *)
(* OpenTheory proof logging                                                  *)
(* Joe Hurd                                                                  *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Logged version of the higher level deductive system.                      *)
(* ------------------------------------------------------------------------- *)

let CONJ_PAIR =
    log_function "CONJ_PAIR" log_thm (log_pair log_thm log_thm) CONJ_PAIR;;

let CONJUNCT1 = log_function "CONJUNCT1" log_thm log_thm CONJUNCT1;;

let CONJUNCT2 = log_function "CONJUNCT2" log_thm log_thm CONJUNCT2;;

let new_inductive_definition =
    log_function "new_inductive_definition"
      log_term (log_triple log_thm log_thm log_thm) new_inductive_definition;;

let new_definition =
    log_function "new_definition" log_term log_thm new_definition;;

let new_specification =
    log_function2 "new_specification"
      (log_list log_name) log_thm log_thm new_specification;;

(***
let new_type =
    log_function "new_type" (log_pair log_name log_num) log_unit new_type;;

let parse_term = log_function "parse_term" log_name log_term parse_term;;

let parse_type = log_function "parse_type" log_name log_type parse_type;;
***)

let prove = log_function "prove" (log_map fst log_term) log_thm prove;;

(* ------------------------------------------------------------------------- *)
(* Delayed version of the higher level deductive system.                     *)
(* ------------------------------------------------------------------------- *)

let EXISTS_TAC = delay_function2 EXISTS_TAC;;

let MESON_TAC = delay_function2 MESON_TAC;;

let REWRITE_TAC = delay_function2 REWRITE_TAC;;

let X_CHOOSE_TAC = delay_function3 X_CHOOSE_TAC;;
