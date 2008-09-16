(* ========================================================================= *)
(* OpenTheory proof logging                                                  *)
(* Joe Hurd                                                                  *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Helper functions                                                          *)
(* ------------------------------------------------------------------------- *)

let curry3 f x y z = f (x,y,z);;

let uncurry3 f (x,y,z) = f x y z;;

(* ------------------------------------------------------------------------- *)
(* Setting up the log files: part 1                                          *)
(* ------------------------------------------------------------------------- *)

let logfile_begin f =
    let h = open_out f in
    output_string h ("# " ^ f ^ "\n");
    h;;

let logfile_end h =
    output_string h ("# eof\n");
    close_out h;;

let logfile_handle = ref (logfile_begin "preamble.art");;

let log_raw s = output_string (!logfile_handle) (s ^ "\n");;

(* ------------------------------------------------------------------------- *)
(* Logging primitive types                                                   *)
(* ------------------------------------------------------------------------- *)

let log_num n = log_raw (string_of_int n);;

let log_name s = log_raw ("\"" ^ String.escaped s ^ "\"");;

let log_command s = log_raw s;;

(* ------------------------------------------------------------------------- *)
(* The dictionary                                                            *)
(* ------------------------------------------------------------------------- *)

let (log_dict_next_key,log_dict_reset_key) =
    let key = ref 0 in
    let next () = let k = !key in (key := k + 1; k) in
    let reset () = key := 0 in
    (next,reset);;

let (log_dict_reset,log_dict_reset_register) =
    let resets = ref [] in
    let register r = resets := r :: !resets in
    let reset_all () = map (fun r -> r ()) (!resets) in
    let reset () = (reset_all (); log_dict_reset_key ()) in
    (reset,register);;

let log_dict (f : ('a -> unit) -> 'a -> unit) =
    let hashtbl = Hashtbl.create 10000 in
    let reset () = Hashtbl.clear hashtbl in
    let () = log_dict_reset_register reset in
    let rec log x =
        if Hashtbl.mem hashtbl x then
          (log_num (Hashtbl.find hashtbl x); log_command "ref")
        else
          (f log x;
           let n = log_dict_next_key () in
           Hashtbl.add hashtbl x n;
           log_num n;
           log_command "def") in
    log;;

(* ------------------------------------------------------------------------- *)
(* Setting up the log files: part 2                                          *)
(* ------------------------------------------------------------------------- *)

let logfile f =
    logfile_end (!logfile_handle);
    log_dict_reset ();
    logfile_handle := logfile_begin f;;

let finish_logging () = logfile_end (!logfile_handle);;

(* ------------------------------------------------------------------------- *)
(* Logging complex types                                                     *)
(* ------------------------------------------------------------------------- *)

let log_map (f : 'a -> 'b) (g : 'b -> unit) x = g (f x);;

let log_list (f : 'a -> unit) =
    let rec logl l =
        match l with
          [] -> log_command "nil"
        | h :: t -> (f h; logl t; log_command "cons") in
    logl;;

let log_unit () = log_list I [];;

let log_sing (f : 'a -> unit) x =
    f x; log_command "nil"; log_command "cons";;

let log_pair (f : 'a -> unit) (g : 'b -> unit) (x,y) =
    f x; log_sing g y; log_command "cons";;

let log_triple (f : 'a -> unit) (g : 'b -> unit) (h : 'c -> unit) (x,y,z) =
    f x; log_pair g h (y,z); log_command "cons";;

let log_call f = log_name f; log_command "call";;

let log_return f = log_name f; log_command "return";;

let log_error () = log_command "error";;

let log_function n log_arg log_ret func arg =
    try
      (log_arg arg;
       log_call n;
       let ret = func arg in
       log_ret ret;
       log_return n;
       ret)
    with Failure err ->
      (log_error ();
       log_return n;
       raise (Failure err));;

let log_function2 n log_arg1 log_arg2 log_ret func =
    curry (log_function n (log_pair log_arg1 log_arg2) log_ret (uncurry func));;

let log_function3 n log_arg1 log_arg2 log_arg3 log_ret func =
    curry3 (log_function n (log_triple log_arg1 log_arg2 log_arg3) log_ret
    (uncurry3 func));;

let log_type =
    let rec log f ty =
        if is_type ty then
          let (n,l) = dest_type ty in
            (log_name n; log_list f l; log_command "type_op")
        else
          (log_name (dest_vartype ty); log_command "type_var") in
    log_dict log;;

let log_var v =
    let (n,ty) = dest_var v in
      (log_name n; log_type ty; log_command "var");;

let log_term =
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

let log_type_inst = log_list (log_pair log_type log_type);;

let log_inst = log_list (log_pair log_term log_term);;

let log_thm =
    let log _ th =
        (log_list log_term (hyp th); log_term (concl th); log_command "thm") in
    log_dict log;;

let log_save_thm th =
    (log_thm th; log_num 0; log_command "dup"; log_command "save");;

(* ------------------------------------------------------------------------- *)
(* Logged version of the logical kernel                                      *)
(* ------------------------------------------------------------------------- *)

let REFL = log_function "REFL" log_term log_thm REFL;;

let TRANS = log_function2 "TRANS" log_thm log_thm log_thm TRANS;;

let MK_COMB =
    log_function "MK_COMB" (log_pair log_thm log_thm) log_thm MK_COMB;;

let ABS = log_function2 "ABS" log_term log_thm log_thm ABS;;

let BETA = log_function "BETA" log_term log_thm BETA;;

let ASSUME = log_function "ASSUME" log_term log_thm ASSUME;;

let EQ_MP = log_function2 "EQ_MP" log_thm log_thm log_thm EQ_MP;;

let DEDUCT_ANTISYM_RULE =
    log_function2 "DEDUCT_ANTISYM_RULE" log_thm log_thm log_thm
    DEDUCT_ANTISYM_RULE;;

let INST_TYPE =
    log_function2 "INST_TYPE" log_type_inst log_thm log_thm INST_TYPE;;

let INST = log_function2 "INST" log_inst log_thm log_thm INST;;

let new_axiom = log_function "new_axiom" log_term log_thm new_axiom;;

let new_basic_definition =
    log_function "new_basic_definition" log_term log_thm new_basic_definition;;

let new_basic_type_definition =
    log_function3 "new_basic_type_definition" log_name
    (log_pair log_name log_name) log_thm (log_pair log_thm log_thm)
    new_basic_type_definition;;

let theorem f = log_function "theorem" log_unit log_save_thm f ();;

let lemma f = log_function "lemma" log_unit log_thm f ();;

(* ------------------------------------------------------------------------- *)
(* Override the loads function to initialize the log files.                  *)
(* ------------------------------------------------------------------------- *)

let loads s =
  let s' = if Filename.is_relative s then Filename.concat (!hol_dir) s
           else s in
  logfile (String.sub s' 0 (String.rindex s' '.') ^ ".art");
  use_file s';;

let loadt s =
  let s' =
    if not (Filename.is_relative s) then s else
    let d = List.find
     (fun d -> Sys.file_exists(Filename.concat d s)) (!load_path) in
    Filename.concat d s in
  use_file s';;
