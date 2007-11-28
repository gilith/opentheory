(* ========================================================================= *)
(* HIGHER ORDER LOGIC SYNTAX                                                 *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Syntax :> Syntax =
struct

open Useful;

structure Ty = Type;
structure T = Term;
structure TAS = TermAlphaSet;
structure TU = TermSubst;

type name = Name.name;
type ty = Ty.ty;
type var = Var.var;
type term = T.term;
type sequent = Sequent.sequent;
type thm = Thm.thm;

(* ------------------------------------------------------------------------- *)
(* Primitive                                                                 *)
(* ------------------------------------------------------------------------- *)

(* Type variables *)

val mk_type_var = Ty.mk_var;
val dest_type_var = Ty.dest_var;
val is_type_var = Ty.is_var;
val equal_type_var = Ty.equal_var;

val alpha = Ty.alpha;

(* Type operators *)

val mk_type_op = Ty.mk_op;
val dest_type_op = Ty.dest_op;
val is_type_op = Ty.is_op;

(* The type of booleans *)

val bool_ty = Ty.bool;

(* Function types *)

val mk_fun = Ty.mk_fun;
val dest_fun = Ty.dest_fun;
val is_fun = Ty.is_fun;

fun list_mk_fun ([],ty) = ty
  | list_mk_fun (x :: xs, ty) = mk_fun (x, list_mk_fun (xs,ty));

local
  fun strip acc ty =
    if not (is_fun ty) then (rev acc, ty)
    else let val (x,ty) = dest_fun ty in strip (x :: acc) ty end;
in
  val strip_fun = strip [];
end;

(* The type of individuals *)

val ind_ty = Ty.ind;

(* Constants *)

val mk_const = T.mk_const;
val dest_const = T.dest_const;
val is_const = T.is_const;

(* Variables *)

val mk_var = T.mk_var;
val dest_var = T.dest_var;
val is_var = T.is_var;
val equal_var = T.equal_var;

(* Function applications *)

val mk_comb = T.mk_comb;
val dest_comb = T.dest_comb;
val is_comb = T.is_comb;

val rator = fst o dest_comb;

val rand = snd o dest_comb;

val land = rand o rator;

fun list_mk_comb (tm,[]) = tm
  | list_mk_comb (tm, x :: xs) = list_mk_comb (mk_comb (tm,x), xs);

local
  fun strip acc tm =
    if not (is_comb tm) then (tm,acc)
    else let val (tm,x) = dest_comb tm in strip (x :: acc) tm end;
in
  val strip_comb = strip [];
end;

(* Lambda abstractions *)

val mk_abs = T.mk_abs;
val dest_abs = T.dest_abs;
val is_abs = T.is_abs;

fun list_mk_abs ([],tm) = tm
  | list_mk_abs (v :: vs, tm) = mk_abs (v, list_mk_abs (vs,tm));

local
  fun strip acc tm =
    if not (is_abs tm) then (rev acc, tm)
    else let val (v,tm) = dest_abs tm in strip (v :: acc) tm end;
in
  val strip_abs = strip [];
end;

(* Equality *)

fun eq_ty a = mk_fun (a, mk_fun (a, bool_ty));

val eq_tm = T.eq;
val mk_eq = T.mk_eq;
val dest_eq = T.dest_eq;
val is_eq = T.is_eq;
val lhs = fst o dest_eq;
val rhs = snd o dest_eq;

(* Hilbert's indefinite choice operator (epsilon) *)

fun select_ty a = mk_fun (mk_fun (a, bool_ty), a);

val select_tm = T.select;
val mk_select = T.mk_select;
val dest_select = T.dest_select;
val is_select = T.is_select;

(* Unary operators *)

fun mk_unop n (ty,a) =
    let
      val c = mk_const (n,ty)
    in
      mk_comb (c,a)
    end;

fun dest_unop n tm =
    let
      val (c,a) = dest_comb tm
      val (n',ty) = dest_const c
      val _ = n = n' orelse raise Error ("dest_unop "^n)
    in
      (ty,a)
    end;

fun is_unop n = can (dest_unop n);

(* Binary operators *)

fun mk_binop n (ty,a,b) =
    let
      val c = mk_const (n,ty)
      val t = mk_comb (c,a)
    in
      mk_comb (t,b)
    end;

fun dest_binop n tm =
    let
      val (t,b) = dest_comb tm
      val (c,a) = dest_comb t
      val (n',ty) = dest_const c
      val _ = n = n' orelse raise Error ("dest_binop "^n)
    in
      (ty,a,b)
    end;

fun is_binop n = can (dest_binop n);

(* Theorems *)

fun thm_id th =
    case Thm.dest th of Thm.Thm {id,...} => id;

fun axioms th =
    case Thm.dest th of Thm.Thm {axioms,...} => axioms;

fun hyp th =
    case Thm.dest th of Thm.Thm {sequent = {hyp, ...}, ...} => hyp;

fun concl th =
    case Thm.dest th of Thm.Thm {sequent = {concl, ...}, ...} => concl;

(* ------------------------------------------------------------------------- *)
(* Boolean                                                                   *)
(* ------------------------------------------------------------------------- *)

(* Negations *)

val negation_name = "~";

fun mk_neg tm = mk_unop negation_name (bool_ty,tm);

fun dest_neg tm =
    let
      val (_,a) = dest_unop negation_name tm
    in
      a
    end;

val is_neg = can dest_neg;

(* Implications *)

val imp_name = "==>";

val mk_imp =
    let
      val imp_ty = mk_fun (bool_ty, mk_fun (bool_ty,bool_ty))
    in
      fn (a,b) => mk_binop imp_name (imp_ty,a,b)
    end;

fun dest_imp tm =
    let
      val (_,a,b) = dest_binop imp_name tm
    in
      (a,b)
    end;

val is_imp = can dest_imp;

(* Universal quantifiers *)

val forall_name = "!";

fun forall_type a = mk_fun (mk_fun (a, bool_ty), bool_ty);

fun mk_forall (v,b) =
    mk_comb (mk_const (forall_name, forall_type (snd v)), mk_abs (v,b));

fun dest_forall tm =
    let
      val (c,t) = dest_comb tm
      val _ = fst (dest_const c) = forall_name orelse raise Error "dest_forall"
    in
      dest_abs t
    end;

val is_forall = can dest_forall;

fun list_mk_forall ([],tm) = tm
  | list_mk_forall (v :: vs, tm) = mk_forall (v, list_mk_forall (vs,tm));

local
  fun strip acc tm =
    if not (is_forall tm) then (rev acc, tm)
    else let val (v,tm) = dest_forall tm in strip (v :: acc) tm end;
in
  val strip_forall = strip [];
end;

(* Existential quantifiers *)

val exists_name = "?";

fun exists_type a = mk_fun (mk_fun (a, bool_ty), bool_ty);

fun mk_exists (v,b) =
    mk_comb (mk_const (exists_name, exists_type (snd v)), mk_abs (v,b));

fun dest_exists tm =
    let
      val (c,t) = dest_comb tm
      val _ = fst (dest_const c) = exists_name orelse raise Error "dest_exists"
    in
      dest_abs t
    end;

val is_exists = can dest_exists;

fun list_mk_exists ([],tm) = tm
  | list_mk_exists (v :: vs, tm) = mk_exists (v, list_mk_exists (vs,tm));

local
  fun strip acc tm =
    if not (is_exists tm) then (rev acc, tm)
    else let val (v,tm) = dest_exists tm in strip (v :: acc) tm end;
in
  val strip_exists = strip [];
end;

(* Unique existential quantifiers *)

val exists_unique_name = "?!";

fun exists_unique_type a = mk_fun (mk_fun (a, bool_ty), bool_ty);

fun mk_exists_unique (v,b) =
    mk_comb (mk_const (exists_unique_name, exists_unique_type (snd v)),
             mk_abs (v,b));

fun dest_exists_unique tm =
    let
      val (c,t) = dest_comb tm
      val _ = fst (dest_const c) = exists_unique_name orelse
              raise Error "dest_exists_unique"
    in
      dest_abs t
    end;

val is_exists_unique = can dest_exists_unique;

fun list_mk_exists_unique ([],tm) = tm
  | list_mk_exists_unique (v :: vs, tm) =
    mk_exists_unique (v, list_mk_exists_unique (vs,tm));

local
  fun strip acc tm =
    if not (is_exists_unique tm) then (rev acc, tm)
    else let val (v,tm) = dest_exists_unique tm in strip (v :: acc) tm end;
in
  val strip_exists_unique = strip [];
end;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing                                                           *)
(* ------------------------------------------------------------------------- *)

(* Names *)

val pp_name = Parser.ppString;

(* Types *)

val type_infix_tokens : Parser.infixities =
  [{token = " * ",   precedence = 3,  leftAssoc = false},
   {token = " + ",   precedence = 2,  leftAssoc = false},
   {token = " -> ",  precedence = 1,  leftAssoc = false}];

local
  val type_infix_strings = Parser.infixTokens type_infix_tokens;

  fun abbreviate_type_op "fun" = "->"
    | abbreviate_type_op n = n;

  val pp_type_var = pp_name;

  val pp_type_op = Parser.ppMap abbreviate_type_op pp_name;

  fun dest_type_infix ty =
      let
        val (f,xs) = dest_type_op ty
        val f = abbreviate_type_op f
        val _ = mem f type_infix_strings orelse raise Error "dest_type_infix"
      in
        case xs of
          [a,b] => (f,a,b)
        | _ => raise Bug ("dest_type_infix: bad arity of type operator " ^ f)
      end;

  val is_type_infix = can dest_type_infix;

  val type_infix_printer =
      Parser.ppInfixes type_infix_tokens (total dest_type_infix);

  fun basic pp ty =
      if is_type_var ty then pp_type_var pp (dest_type_var ty)
      else if is_type_infix ty then pp_btype pp ty
      else
        let
          val (f,xs) = dest_type_op ty
        in
          Parser.beginBlock pp Parser.Inconsistent 0;
          (case xs of
             [] => ()
           | [x] => (basic pp ty; Parser.addBreak pp (1,0))
           | _ =>
             (Parser.ppBracket "(" ")" (Parser.ppSequence "," pp_type) pp xs;
              Parser.addBreak pp (1,0)));
          pp_type_op pp f;
          Parser.endBlock pp
        end

  and basicr pp (ty,_) = basic pp ty

  and pp_btype pp ty = Parser.ppBracket "(" ")" pp_type pp ty

  and pp_typer pp tyr = type_infix_printer basicr pp tyr

  and pp_type pp ty = pp_typer pp (ty,false);
in
  val pp_type = pp_type;
end;

val type_to_string = Parser.toString pp_type;

(* Terms *)

val SHOW_TYPES = ref false;

val infix_tokens : Parser.infixities =
  [(* ML style *)
   {token = " / ",   precedence = 7,  leftAssoc = true},
   {token = " div ", precedence = 7,  leftAssoc = true},
   {token = " mod ", precedence = 7,  leftAssoc = true},
   {token = " * ",   precedence = 7,  leftAssoc = true},
   {token = " + ",   precedence = 6,  leftAssoc = true},
   {token = " - ",   precedence = 6,  leftAssoc = true},
   {token = " ^ ",   precedence = 6,  leftAssoc = true},
   {token = " @ ",   precedence = 5,  leftAssoc = false},
   {token = " :: ",  precedence = 5,  leftAssoc = false},
   {token = " = ",   precedence = 4,  leftAssoc = true},
   {token = " <> ",  precedence = 4,  leftAssoc = true},
   {token = " <= ",  precedence = 4,  leftAssoc = true},
   {token = " < ",   precedence = 4,  leftAssoc = true},
   {token = " >= ",  precedence = 4,  leftAssoc = true},
   {token = " > ",   precedence = 4,  leftAssoc = true},
   {token = " o ",   precedence = 3,  leftAssoc = true},
   (* HOL style *)
   {token = " /\\ ", precedence = ~1, leftAssoc = false},
   {token = " \\/ ", precedence = ~2, leftAssoc = false},
   {token = " ==> ", precedence = ~3, leftAssoc = false},
   {token = " <=> ", precedence = ~4, leftAssoc = false}];

val pp_var =
    let
      val pp1 = Parser.ppBracket "(" ")" (Parser.ppBinop " :" pp_name pp_type)
      val pp2 = Parser.ppMap fst pp_name
    in
      fn pp => fn v => (if !SHOW_TYPES then pp1 else pp2) pp v
    end;

local
  val binders =
      [("\\",strip_abs),
       ("!",strip_forall),
       ("?",strip_exists),
       ("?!",strip_exists_unique)];

  val infix_strings = Parser.infixTokens infix_tokens;

  val binder_strings = map fst binders;

  fun abbreviate_const n = n;

  fun special_string n =
      n = "~" orelse mem n infix_strings orelse mem n binder_strings;

  val pp_const =
      let
        fun f (n,_) =
            let
              val n = abbreviate_const n
            in
              if special_string n then "(" ^ n ^ ")" else n
            end
      in
        Parser.ppMap f pp_name
      end;

  fun dest_infix tm =
      let
        val (t,b) = dest_comb tm
        val (c,a) = dest_comb t
        val (n,_) = dest_const c
        val n = abbreviate_const n
      in
        if mem n infix_strings then (n,a,b) else raise Error "HOL.dest_infix"
      end;

  val is_infix = can dest_infix;

  fun count_negations tm =
      case total dest_neg tm of
        NONE => (0,tm)
      | SOME t => let val (n,r) = count_negations t in (n + 1, r) end;

  fun dest_binder tm =
      let
        fun f (s,d) = case d tm of ([],_) => NONE | (vs,b) => SOME (s,vs,b)
      in
        case first f binders of
          SOME x => x
        | NONE => raise Error "Syntax.dest_binder"
      end;

  val is_binder = can dest_binder;

  val infix_printer = Parser.ppInfixes infix_tokens (total dest_infix);

  fun basic pp tm =
      if is_var tm then pp_var pp (dest_var tm)
      else if is_const tm then pp_const pp (dest_const tm)
      else pp_btm pp tm

  and application pp tm =
      let
        val (f,xs) = strip_comb tm
      in
        basic pp f;
        app (fn x => (Parser.addBreak pp (1,0); basic pp x)) xs
      end

  and binder pp (tm,r) =
      let
        fun pp_bind pp tm =
            let
              val (sym,vs,body) = dest_binder tm
              val (v,vs) = hdTl vs
            in
              Parser.addString pp sym;
              pp_var pp v;
              app (fn a => (Parser.addBreak pp (1,0); pp_var pp a)) vs;
              Parser.addString pp ".";
              Parser.addBreak pp (1,0);
              if is_binder body then pp_bind pp body else pp_tm pp (body,false)
            end

        fun pp_binder pp tm =
            (Parser.beginBlock pp Parser.Inconsistent 2;
             pp_bind pp tm;
             Parser.endBlock pp)
      in
        (if not (is_binder tm) then application
         else (if r then Parser.ppBracket "(" ")" else I) pp_binder) pp tm
      end

  and negations pp (tm,r) =
      let
        val (n,tm) = count_negations tm
      in
        Parser.beginBlock pp Parser.Inconsistent n;
        funpow n (fn () => Parser.addString pp "~") ();
        if is_infix tm then pp_btm pp tm else binder pp (tm,r);
        Parser.endBlock pp
      end

  and pp_btm pp tm = Parser.ppBracket "(" ")" pp_tm pp (tm,false)

  and pp_tm pp tmr = infix_printer negations pp tmr;
in
  val pp_term = Parser.ppMap (fn tm => (tm,false)) pp_tm;
end;

val term_to_string = Parser.toString pp_term;

(* Substitutions *)

val pp_subst =
    Parser.ppMap
      (fn sub => (TU.to_list_type sub, TU.to_list sub))
      (Parser.ppPair (Parser.ppList (Parser.ppPair pp_name pp_type))
                     (Parser.ppList (Parser.ppPair pp_var pp_term)));

val subst_to_string = Parser.toString pp_subst;

(* Sequents and theorems *)

local
  fun pp_seq pp binop hyp concl =
      (Parser.beginBlock pp Parser.Inconsistent 2;
       if TAS.null hyp then ()
       else
         (Parser.ppBracket "{" "}"
            (Parser.ppSequence "," pp_term) pp (TAS.toList hyp);
          Parser.addBreak pp (1,0));
       Parser.addString pp (binop ^ " ");
       pp_term pp concl;
       Parser.endBlock pp);
in
  fun pp_sequent pp {hyp,concl} = pp_seq pp "?-" hyp concl;

  fun pp_thm pp th =
      case Thm.dest th of
        Thm.Thm {sequent = {hyp,concl}, ...} => pp_seq pp "|-" hyp concl;
end;

val sequent_to_string = Parser.toString pp_sequent;

val thm_to_string = Parser.toString pp_thm;

end
