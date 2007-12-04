(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful Syntax Rule;

structure S = Binaryset;
structure M = Binarymap;
structure IS = Intset;
structure IM = Intmap;

structure N = Name;
structure NS = NameSet;
structure NM = NameMap;
structure Ty = Type;
structure T = Term;
structure TAS = TermAlphaSet;
structure TU = TermSubst;

infix |-> ## ==

val op== = Portable.pointerEqual;

(* ------------------------------------------------------------------------- *)
(* Helper functions                                                          *)
(* ------------------------------------------------------------------------- *)

fun plural 1 s = "1 " ^ s
  | plural n s = Int.toString n ^ " " ^ s ^ "s";

fun nat_from_string err s =
    case Int.fromString s of
      SOME i => i
    | NONE => raise Error err;

(* ------------------------------------------------------------------------- *)
(* Bilingual dictionaries                                                    *)
(* ------------------------------------------------------------------------- *)

datatype bilingual =
    Bilingual of
    {export : name NM.map,
     import : name NM.map};

val bilingual_empty = Bilingual {export = NM.new (), import = NM.new ()};

fun bilingual_add (a,b) (Bilingual {export,import}) =
    let
      fun unseen x m = not (NM.inDomain x m)
      val _ = unseen a export orelse raise Error "export conflict"
      val _ = unseen b import orelse raise Error "import conflict"
      val export = NM.insert export (a,b)
      val import = NM.insert import (b,a)
    in
      Bilingual {export = export, import = import}
    end
    handle Error err => raise Error ("bilingual_add: " ^ err);

fun bilingual_addl l b = foldl (uncurry bilingual_add) b l;

fun bilingual_export (Bilingual {export,...}) a =
    Option.getOpt (NM.peek export a, a);

fun bilingual_import (Bilingual {import,...}) b =
    Option.getOpt (NM.peek import b, b);

(* ------------------------------------------------------------------------- *)
(* Objects                                                                   *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Oerror
  | Onum of int
  | Oname of name
  | Olist of object list
  | Otype of ty
  | Oterm of term
  | Othm of thm
  | Ocall of name;

fun dest_oerror Oerror = () | dest_oerror _ = raise Error "dest_oerror";
val is_oerror = can dest_oerror;

fun dest_onum (Onum n) = n | dest_onum _ = raise Error "dest_onum";
val is_onum = can dest_onum;

fun dest_oname (Oname n) = n | dest_oname _ = raise Error "dest_oname";
val is_oname = can dest_oname;

fun dest_olist (Olist l) = l | dest_olist _ = raise Error "dest_olist";
val is_olist = can dest_olist;

fun dest_otype (Otype ty) = ty | dest_otype _ = raise Error "dest_otype";
val is_otype = can dest_otype;

fun dest_oterm (Oterm tm) = tm | dest_oterm _ = raise Error "dest_oterm";
val is_oterm = can dest_oterm;

fun dest_othm (Othm th) = th | dest_othm _ = raise Error "dest_othm";
val is_othm = can dest_othm;

fun dest_ocall (Ocall n) = n | dest_ocall _ = raise Error "dest_ocall";
val is_ocall = can dest_ocall;

fun mk_ounit () = Olist [];

fun mk_opair (x,y) = Olist [x,y];
fun dest_opair (Olist [x,y]) = (x,y) | dest_opair _ = raise Error "dest_opair";
val is_opair = can dest_opair;

fun dest_otriple (Olist [x,y,z]) = (x,y,z)
  | dest_otriple _ = raise Error "dest_otriple";
val is_otriple = can dest_otriple;

val dest_ovar = (dest_oname ## dest_otype) o dest_opair;
val is_ovar = can dest_ovar;

fun object_compare ob1_ob2 =
    if op== ob1_ob2 then EQUAL
    else
      case ob1_ob2 of
        (Oerror,Oerror) => EQUAL
      | (Oerror,_) => LESS
      | (_,Oerror) => GREATER
      | (Onum n1, Onum n2) => Int.compare (n1,n2)
      | (Onum _, _) => LESS
      | (_, Onum _) => GREATER
      | (Oname n1, Oname n2) => N.compare (n1,n2)
      | (Oname _, _) => LESS
      | (_, Oname _) => GREATER
      | (Otype ty1, Otype ty2) => Ty.compare (ty1,ty2)
      | (Otype _, _) => LESS
      | (_, Otype _) => GREATER
      | (Oterm tm1, Oterm tm2) => T.compare (tm1,tm2)
      | (Oterm _, _) => LESS
      | (_, Oterm _) => GREATER
      | (Othm th1, Othm th2) => Int.compare (thm_id th1, thm_id th2)
      | (Othm _, _) => LESS
      | (_, Othm _) => GREATER
      | (Olist l1, Olist l2) => lexCompare object_compare (l1,l2)
      | (Olist _, _) => LESS
      | (_, Olist _) => GREATER
      | (Ocall n1, Ocall n2) => N.compare (n1,n2);

fun pp_object pp ob =
    case ob of
      Oerror => Parser.ppString pp "ERROR"
    | Onum n => Parser.ppInt pp n
    | Oname s => Parser.ppString pp ("\"" ^ s ^ "\"")
    | Otype ty => pp_type pp ty
    | Oterm tm => pp_term pp tm
    | Othm th => pp_thm pp th
    | Olist l => Parser.ppList pp_object pp l
    | Ocall f => Parser.ppString pp ("<" ^ f ^ ">");

val extract_thms =
    let
      fun f acc [] = acc
        | f acc (Othm th :: rest) = f (th :: acc) rest
        | f acc (Olist l :: rest) = f acc (l @ rest)
        | f acc (_ :: rest) = f acc rest
    in
      f []
    end;

(* ------------------------------------------------------------------------- *)
(* Translations                                                              *)
(* ------------------------------------------------------------------------- *)

fun import_into_namespace namespace name = join "." (namespace @ [name]);

fun export_from_namespace namespace name =
    foldl (fn (m,x) => Option.getOpt (total (destPrefix (m ^ ".")) x, x))
    name namespace;

datatype translation =
    Translation of
    {namespace : name list,
     types : bilingual,
     consts : bilingual,
     rules : bilingual,
     simulations : (object -> object) NM.map};

local
  fun import namespace bilingual name =
      bilingual_import bilingual (import_into_namespace namespace name);

  fun export namespace bilingual name =
      export_from_namespace namespace (bilingual_export bilingual name);
in
  fun import_type (Translation {namespace,types,...}) = import namespace types;
  fun export_type (Translation {namespace,types,...}) = export namespace types;

  fun import_const (Translation {namespace = n, consts, ...}) = import n consts;
  fun export_const (Translation {namespace = n, consts, ...}) = export n consts;

  fun import_rule (Translation {namespace,rules,...}) = import namespace rules;
  fun export_rule (Translation {namespace,rules,...}) = export namespace rules;
end;

local
  fun mk_trans l = bilingual_addl l bilingual_empty;
in
  fun mk_translation {namespace,types,consts,rules} =
      Translation
        {namespace = namespace,
         types = mk_trans types,
         consts = mk_trans consts,
         rules = mk_trans rules,
         simulations = NM.new ()};
end;

fun add_simulations l translation =
    let
      val Translation {namespace,types,consts,rules,simulations} = translation
      val simulations = foldl (fn (x,z) => NM.insert z x) simulations l
    in
      Translation
        {namespace = namespace,
         types = types,
         consts = consts,
         rules = rules,
         simulations = simulations}
    end;

fun translation_simulate (Translation {simulations,...}) name =
    case NM.peek simulations name of
      NONE => K NONE
    | SOME sim => SOME o sim;

(* ------------------------------------------------------------------------- *)
(* The natural translation                                                   *)
(* ------------------------------------------------------------------------- *)

val natural =
    mk_translation
    {namespace = [],
     types = [],
     consts = [],
     rules = []};

(* ------------------------------------------------------------------------- *)
(* Stacks                                                                    *)
(* ------------------------------------------------------------------------- *)

type stack = object list;

val empty_stack : stack = [];

fun top_call [] = NONE
  | top_call (Ocall n :: l) = SOME (n,l)
  | top_call (_ :: l) = top_call l;

val call_stack = List.mapPartial (total dest_ocall);

(* ------------------------------------------------------------------------- *)
(* Dictionaries                                                              *)
(* ------------------------------------------------------------------------- *)

type dict = object Intmap.intmap;

val empty_dict : dict = Intmap.empty ();

fun dict_def (n,obj) dict = Intmap.insert (dict,n,obj);

fun dict_ref dict n =
    case Intmap.peek (dict,n) of
      SOME obj => obj
    | NONE => raise Error ("dict_ref: no entry for number "^Int.toString n);

(* ------------------------------------------------------------------------- *)
(* Saved theorems                                                            *)
(* ------------------------------------------------------------------------- *)

datatype saved = Saved of thm list;

fun saved_list (Saved l) = rev l;

val saved_empty = Saved [];

fun saved_add th (Saved l) = Saved (th :: l);

(* ------------------------------------------------------------------------- *)
(* Sets of theorems                                                          *)
(* ------------------------------------------------------------------------- *)

datatype thm_set = ThmSet of (term list, thm) M.dict;

local
  fun mk_key (h,c) = c :: TAS.toList h;

  fun thm_to_key th = mk_key (hyp th, concl th);

  fun sequent_to_key (h,c) = mk_key (TAS.fromList h, c);
in
  val thm_set_empty = ThmSet (M.mkDict (lexCompare T.alpha_compare));

  fun thm_set_peek (ThmSet m) h_c = M.peek (m, sequent_to_key h_c);

  fun thm_set_add th (ThmSet m) = ThmSet (M.insert (m, thm_to_key th, th));
end;

fun thm_set_add_list ths set = foldl (fn (t,s) => thm_set_add t s) set ths;

(* ------------------------------------------------------------------------- *)
(* The theorem dependency graph                                              *)
(* ------------------------------------------------------------------------- *)

datatype thm_deps = ThmDeps of int list IM.intmap;

val thm_deps_empty = ThmDeps (IM.empty ());

fun thm_deps_add (th,thl) thm_deps =
    let
      val ThmDeps m = thm_deps
      val i = thm_id th
      and il = map thm_id thl
      val _ = List.all (fn k => k <= i) il orelse
              raise Bug ("thm_deps_add: bad deps for\n"^thm_to_string th)
    in
      if mem i il then thm_deps
      else
        let
          val _ = not (Option.isSome (IM.peek (m,i))) orelse
                  raise Bug ("thm_deps_add: new deps for\n"^thm_to_string th)
        in
          ThmDeps (IM.insert (m,i,il))
        end
    end;

fun thm_deps_add_list xs deps = foldl (fn (x,d) => thm_deps_add x d) deps xs;

fun thm_deps_useful (ThmDeps m) =
    let
      fun f [] set = set
        | f (i :: rest) set =
          if IS.member (set,i) then f rest set
          else
            case IM.peek (m,i) of
              SOME il => f (il @ rest) (IS.add (set,i))
            | NONE => raise Bug ("thm_deps_useful: no dep for "^Int.toString i)
    in
      fn ths => f (map thm_id ths) IS.empty
    end;

(* ------------------------------------------------------------------------- *)
(* Articles                                                                  *)
(* ------------------------------------------------------------------------- *)

datatype article_op =
         Function_call of {name : name,
                           arg : object,
                           ops : article_op list,
                           ret : object}
       | Save_thm of thm;
         
fun function_call_update_ops ops artop =
    case artop of
      Function_call {name,arg,ret,...} =>
      Function_call {name = name, arg = arg, ops = ops, ret = ret}
    | Save_thm _ => raise Error "function_call_update_ops";

datatype article = Article of article_op list;

val article_empty = Article [];

fun article_append (Article x) (Article y) = Article (x @ y);

val article_concat = foldl (fn (x,z) => article_append z x) article_empty;

val article_operations =
    let
      fun f n [] = n
        | f n (Function_call {ops,...} :: rest) = f (n + 1) (ops @ rest)
        | f n (Save_thm _ :: rest) = f (n + 1) rest
    in
      fn Article l => f 0 l
    end;

fun article_to_string article =
    "[" ^ Int.toString (article_operations article) ^ "]";

val pp_article = Parser.ppMap article_to_string Parser.ppString;

local
  fun special_filter _ [] s = ([],s)
    | special_filter p (h :: t) s =
      case p h s of
        SOME (h,s) =>
        let
          val (t,s) = special_filter p t s
        in
          (h :: t, s)
        end
      | NONE => special_filter p t s;

  val delete_thms =
      let
        fun f (i,(b,s)) =
            if IS.member (s,i) then (true, IS.delete (s,i)) else (b,s)
      in
        fn ths => fn set => foldl f (false,set) (map thm_id ths)
      end;

  fun eliml ops useful = special_filter elim ops useful
  and elim (x as Save_thm _) useful = SOME (x,useful)
    | elim (x as Function_call {ops,ret,...}) useful =
      let
        val (ops,useful) = eliml ops useful
        val (ok,useful) = delete_thms (extract_thms [ret]) useful
      in
        if not (ok orelse not (null ops)) then NONE
        else SOME (function_call_update_ops ops x, useful)
      end;
in
  fun dead_theorem_elimination useful article = 
    let
(*TRACE1
      val _ = trace ("dead_theorem_elimination: before = " ^
                     article_to_string article ^ "\n")
*)
      val Article ops = article
      val (ops,useful) = eliml ops useful
      val article = Article ops
(*TRACE1
      val _ = trace ("dead_theorem_elimination: after = " ^
                     article_to_string article ^ "\n")
*)
    in
      article
    end;
end;

(* ------------------------------------------------------------------------- *)
(* hol-light                                                                 *)
(* ------------------------------------------------------------------------- *)

val hol_light =
    ref
      (mk_translation
         {namespace = ["hol-light"],
          types = [("fun","hol-light.fun"),
                   ("bool","hol-light.bool")],
          consts = [("=","hol-light.="),
                    ("@","hol-light.@"),
                    ("T","hol-light.T"),
                    ("F","hol-light.F"),
                    ("~","hol-light.~"),
                    ("/\\","hol-light./\\"),
                    ("\\/","hol-light.\\/"),
                    ("==>","hol-light.==>"),
                    ("!","hol-light.!"),
                    ("?","hol-light.?"),
                    ("?!","hol-light.?!")],
          rules = []});

fun hol_light_type_subst_to_subst oins =
    let
      fun f (x,y) = (dest_type_var (dest_otype y), dest_otype x)
      val l = dest_olist oins
    in
      TU.from_list_type (map (f o dest_opair) l)
    end
    handle Error err =>
      raise Bug ("hol_light_type_subst_to_subst failed:\n" ^ err);

fun hol_light_subst_to_subst oins =
    let
      fun f (x,y) = (dest_var (dest_oterm y), dest_oterm x)
      val l = dest_olist oins
    in
      TU.from_list (map (f o dest_opair) l)
    end
    handle Error err =>
      raise Bug ("hol_light_subst_to_subst failed:\n" ^ err);

fun hol_light_new_basic_definition arg =
    let
      val tm = dest_oterm arg
      val (v,t) = dest_eq tm
      val (n,ty) = dest_var v
      val n = import_const (!hol_light) n
      val v = mk_var (n,ty)
      val tm = mk_eq (v,t)
    in
      Othm (Define tm)
    end
    handle Error err =>
      raise Bug ("hol_light_new_basic_definition failed:\n" ^ err);

fun hol_light_new_basic_type_definition arg =
    let
      val (name,abs_rep,non_empty_th) = dest_otriple arg
      val name = import_type (!hol_light) (dest_oname name)
      val (abs,rep) = dest_opair abs_rep
      val abs = import_const (!hol_light) (dest_oname abs)
      and rep = import_const (!hol_light) (dest_oname rep)
      and non_empty_th = dest_othm non_empty_th
      val ty_vars = NS.toList (T.type_vars (concl non_empty_th))
      val (abs_rep_th,rep_abs_th) =
          Define_type name {abs = abs, rep = rep} ty_vars non_empty_th
    in
      mk_opair (Othm abs_rep_th, Othm rep_abs_th)
    end
    handle Error err =>
      raise Bug ("hol_light_new_basic_type_definition failed:\n" ^ err);

fun hol_light_ABS arg =
    let
      val (otm,oth) = dest_opair arg
      val v = dest_var (dest_oterm otm)
      val th = dest_othm oth
    in
      Othm (Abs v th)
    end;

fun hol_light_ASSUME arg = Othm (Assume (dest_oterm arg));

fun hol_light_BETA arg = Othm (Beta_conv (dest_oterm arg));

fun hol_light_DEDUCT_ANTISYM_RULE arg =
    let
      val (oth1,oth2) = dest_opair arg
      val th1 = dest_othm oth1
      val th2 = dest_othm oth2
    in
      Othm (Deduct_antisym th1 th2)
    end;

fun hol_light_EQ_MP arg =
    let
      val (oth1,oth2) = dest_opair arg
      val th1 = dest_othm oth1
      val th2 = dest_othm oth2
    in
      Othm (Eq_mp th1 th2)
    end;

fun hol_light_INST arg =
    let
      val (oins,oth) = dest_opair arg
      val ins = hol_light_subst_to_subst oins
      val th = dest_othm oth
    in
      Othm (Subst ins th)
    end;

fun hol_light_INST_TYPE arg =
    let
      val (oins,oth) = dest_opair arg
      val ins = hol_light_type_subst_to_subst oins
      val th = dest_othm oth
    in
      Othm (Subst ins th)
    end;

fun hol_light_MK_COMB arg =
    let
      val (oth1,oth2) = dest_opair arg
      val th1 = dest_othm oth1
      val th2 = dest_othm oth2
    in
      Othm (Mk_comb th1 th2)
    end;

fun hol_light_REFL arg = Othm (Refl (dest_oterm arg));

fun hol_light_TRANS arg =
    let
      val (oth1,oth2) = dest_opair arg
      val th1 = dest_othm oth1
      val th2 = dest_othm oth2
    in
      Othm (Trans th1 th2)
    end;

val () =
    hol_light :=
    add_simulations
      [("hol-light.new_basic_definition", hol_light_new_basic_definition),
       ("hol-light.new_basic_type_definition",
        hol_light_new_basic_type_definition),
       ("hol-light.ABS", hol_light_ABS),
       ("hol-light.ASSUME", hol_light_ASSUME),
       ("hol-light.BETA", hol_light_BETA),
       ("hol-light.DEDUCT_ANTISYM_RULE", hol_light_DEDUCT_ANTISYM_RULE),
       ("hol-light.EQ_MP", hol_light_EQ_MP),
       ("hol-light.INST", hol_light_INST),
       ("hol-light.INST_TYPE", hol_light_INST_TYPE),
       ("hol-light.MK_COMB", hol_light_MK_COMB),
       ("hol-light.REFL", hol_light_REFL),
       ("hol-light.TRANS", hol_light_TRANS)]
      (!hol_light);

(* ------------------------------------------------------------------------- *)
(* Getting hold of required theorems by hook or by crook                     *)
(* ------------------------------------------------------------------------- *)

datatype particle =
         Particle of {name : name,
                      arg : object,
                      rev_ops : article_op list,
                      thms : thm_set};

datatype extra =
         Extra of {particles : particle * particle list,
                   new_types : NS.set,
                   new_consts : NS.set,
                   new_axioms : thm list,
                   saved_thms : thm_set,
                   thm_deps : thm_deps};

fun particle_name (Particle {name,...}) = name;

fun particle_arg (Particle {arg,...}) = arg;

fun particle_rev_ops (Particle {rev_ops,...}) = rev_ops;

fun particle_thms (Particle {thms,...}) = thms;

fun particle_update_name name particle =
    let
      val Particle {arg,rev_ops,thms,...} = particle
    in
      Particle {name = name, arg = arg, rev_ops = rev_ops, thms = thms}
    end;

fun particle_update_arg arg particle =
    let
      val Particle {name,rev_ops,thms,...} = particle
    in
      Particle {name = name, arg = arg, rev_ops = rev_ops, thms = thms}
    end;

fun particle_update_rev_ops rev_ops particle =
    let
      val Particle {name,arg,thms,...} = particle
    in
      Particle {name = name, arg = arg, rev_ops = rev_ops, thms = thms}
    end;

fun particle_update_thms thms particle =
    let
      val Particle {name,arg,rev_ops,...} = particle
    in
      Particle {name = name, arg = arg, rev_ops = rev_ops, thms = thms}
    end;

fun mk_particle name arg thms =
    Particle {name = name, arg = arg, rev_ops = [], thms = thms};

fun particle_ops particle = rev (particle_rev_ops particle);

fun particle_add_op op' particle =
    particle_update_rev_ops (op' :: particle_rev_ops particle) particle;

fun extra_particles (Extra {particles,...}) = particles;

fun extra_new_types (Extra {new_types,...}) = new_types;

fun extra_new_consts (Extra {new_consts,...}) = new_consts;

fun extra_new_axioms (Extra {new_axioms,...}) = new_axioms;

fun extra_saved_thms (Extra {saved_thms,...}) = saved_thms;

fun extra_thm_deps (Extra {thm_deps,...}) = thm_deps;

fun extra_update_particles particles extra =
    let
      val Extra {new_types, new_consts, new_axioms,
                 saved_thms, thm_deps, ...} = extra
    in
      Extra
        {particles = particles, new_types = new_types,
         new_consts = new_consts, new_axioms = new_axioms,
         saved_thms = saved_thms, thm_deps = thm_deps}
    end;

fun extra_update_new_types new_types extra =
    let
      val Extra {particles, new_consts, new_axioms,
                 saved_thms, thm_deps, ...} = extra
    in
      Extra
        {particles = particles, new_types = new_types,
         new_consts = new_consts, new_axioms = new_axioms,
         saved_thms = saved_thms, thm_deps = thm_deps}
    end;

fun extra_update_new_consts new_consts extra =
    let
      val Extra {particles, new_types, new_axioms,
                 saved_thms, thm_deps, ...} = extra
    in
      Extra
        {particles = particles, new_types = new_types,
         new_consts = new_consts, new_axioms = new_axioms,
         saved_thms = saved_thms, thm_deps = thm_deps}
    end;

fun extra_update_new_axioms new_axioms extra =
    let
      val Extra {particles, new_types, new_consts,
                 saved_thms, thm_deps, ...} = extra
    in
      Extra
        {particles = particles, new_types = new_types,
         new_consts = new_consts, new_axioms = new_axioms,
         saved_thms = saved_thms, thm_deps = thm_deps}
    end;

fun extra_update_saved_thms saved_thms extra =
    let
      val Extra {particles, new_types, new_consts,
                 new_axioms, thm_deps, ...} = extra
    in
      Extra
        {particles = particles, new_types = new_types,
         new_consts = new_consts, new_axioms = new_axioms,
         saved_thms = saved_thms, thm_deps = thm_deps}
    end;

fun extra_update_thm_deps thm_deps extra =
    let
      val Extra {particles, new_types, new_consts,
                 new_axioms, saved_thms, ...} = extra
    in
      Extra
        {particles = particles, new_types = new_types,
         new_consts = new_consts, new_axioms = new_axioms,
         saved_thms = saved_thms, thm_deps = thm_deps}
    end;

fun extra_name extra =
    let
      val (top_particle,_) = extra_particles extra
    in
      particle_name top_particle
    end;

fun extra_arg extra =
    let
      val (top_particle,_) = extra_particles extra
    in
      particle_arg top_particle
    end;

fun extra_thms extra =
    let
      val (top_particle,_) = extra_particles extra
    in
      particle_thms top_particle
    end;

fun extra_update_thms thms extra =
    let
      val (top_particle,particles) = extra_particles extra
      val top_particle = particle_update_thms thms top_particle
    in
      extra_update_particles (top_particle,particles) extra
    end;

fun extra_thms_add_list thl extra =
    let
      val thms = extra_thms extra
      val thms = thm_set_add_list thl thms
    in
      extra_update_thms thms extra
    end;

fun extra_add_new_type n extra =
    let
      val new_types = extra_new_types extra
      val new_types = NS.add new_types n
    in
      extra_update_new_types new_types extra
    end;

fun extra_add_new_const n extra =
    let
      val new_consts = extra_new_consts extra
      val new_consts = NS.add new_consts n
    in
      extra_update_new_consts new_consts extra
    end;

fun extra_add_new_axiom th extra =
    let
      val new_axioms = extra_new_axioms extra
      val new_axioms = th :: new_axioms
    in
      extra_update_new_axioms new_axioms extra
    end;

fun extra_add_op op' extra =
    let
      val (top_particle,particles) = extra_particles extra
      val top_particle = particle_add_op op' top_particle
    in
      extra_update_particles (top_particle,particles) extra
    end;

fun extra_thm_deps_add deps extra =
    let
      val thm_deps = extra_thm_deps extra
      val thm_deps = thm_deps_add deps thm_deps
    in
      extra_update_thm_deps thm_deps extra
    end;

fun extra_thm_deps_add_list deps extra =
    let
      val thm_deps = extra_thm_deps extra
      val thm_deps = thm_deps_add_list deps thm_deps
    in
      extra_update_thm_deps thm_deps extra
    end;

val pp_extra = Parser.ppMap (fn _ : extra => "<extra>") Parser.ppString;

val extra_init =
    Extra
      {particles = (mk_particle "<main>" (mk_ounit ()) thm_set_empty, []),
       new_types = NS.empty,
       new_consts = NS.empty,
       new_axioms = [],
       saved_thms = thm_set_empty,
       thm_deps = thm_deps_empty};
    
fun extra_find_theorem ths h_c extra =
    case thm_set_peek ths h_c of
      NONE => NONE
    | SOME th =>
      let
        val th' = Alpha h_c th
        val extra = extra_thm_deps_add (th',[th]) extra
      in
        SOME (th',extra)
      end;

fun extra_simulate translation extra =
    let
      val arg = extra_arg extra
    in
      case translation_simulate translation (extra_name extra) arg of
        NONE => extra
      | SOME result =>
        let
          val result_thl = extract_thms [result]
          val extra = extra_thms_add_list result_thl extra
          val arg_thl = extract_thms [arg]
          val new_deps = map (fn th => (th,arg_thl)) result_thl
          val extra = extra_thm_deps_add_list new_deps extra
        in
          extra
        end
    end;

local
  val type_op_exists = can Ty.type_arity;

  fun extra_new_type_op (n,arity) extra =
      let
        val () = warn ("making new type operator " ^ n)
        val () = Ty.declare_type n arity
      in
        extra_add_new_type n extra
      end;
in
  fun extra_type_op translation (n,l) extra =
      let
        val extra =
            if type_op_exists n then extra
            else
              let
                val extra = extra_simulate translation extra
              in
                if type_op_exists n then extra
                else extra_new_type_op (n, length l) extra
              end
      in
        (mk_type_op (n,l), extra)
      end;
end;

local
  val const_exists = can T.const_type;

  fun extra_new_const n extra =
      let
        val () = warn ("making new constant " ^ n)
        val () = T.declare_const n alpha
      in
        extra_add_new_const n extra
      end;
in
  fun extra_const translation (n,ty) extra =
      let
        val extra =
            if const_exists n then extra
            else
              let
                val extra = extra_simulate translation extra
              in
                if const_exists n then extra
                else extra_new_const n extra
              end
      in
        (mk_const (n,ty), extra)
      end;
end;

fun extra_axiom (h,c) extra =
    let
      val th = Axiom {hyp = TAS.fromList h, concl = c}
      val () = warn ("making new axiom:\n" ^ thm_to_string th)
      val extra = extra_add_new_axiom th extra
      val extra = extra_thm_deps_add (th,[]) extra
    in
      (th,extra)
    end;

fun extra_thm translation h_c extra =
    case extra_find_theorem (extra_saved_thms extra) h_c extra of
      SOME th_extra => th_extra
    | NONE =>
      case extra_find_theorem (extra_thms extra) h_c extra of
        SOME th_extra => th_extra
      | NONE =>
        let
          val extra = extra_simulate translation extra
        in
          case extra_find_theorem (extra_thms extra) h_c extra of
            SOME th_extra => th_extra
          | NONE => extra_axiom h_c extra
        end;

fun extra_call name arg extra =
    let
      val (top_particle,particles) = extra_particles extra
      val new_particle = mk_particle name arg (particle_thms top_particle)
    in
      extra_update_particles (new_particle, top_particle :: particles) extra
    end;

fun extra_return name ret extra =
    let
      val (top_particle,particles) = extra_particles extra
      val (next_particle,particles) =
          case particles of
            [] => raise Bug "extra_return: bad particle list"
          | next :: rest => (next,rest)
      val new_function_call =
          let
            val _ = name = particle_name top_particle orelse
                    raise Bug "extra_return: name mismatch"
            val arg = particle_arg top_particle
            and ops = particle_ops top_particle
          in
            Function_call {name = name, arg = arg, ops = ops, ret = ret}
          end
      val extra = extra_update_particles (next_particle,particles) extra
      val extra = extra_add_op new_function_call extra
      val thms = thm_set_add_list (extract_thms [ret]) (extra_thms extra)
      val extra = extra_update_thms thms extra
    in
      extra
    end;

fun extra_save th extra = extra_add_op (Save_thm th) extra;

fun extra_final saved extra =
    let
      val particle =
          case extra_particles extra of
            (top_particle,[]) => top_particle
          | (_, _ :: _) => raise Error "article ended inside a function call"
      val _ = particle_name particle = "<main>" orelse
              raise Bug "extra_final: bad particle name"
      val article = Article (particle_ops particle)
      val saved = saved_list saved
      val useful = thm_deps_useful (extra_thm_deps extra) saved
      val article = dead_theorem_elimination useful article
    in
      (saved,article)
    end;

(* ------------------------------------------------------------------------- *)
(* I/O                                                                       *)
(* ------------------------------------------------------------------------- *)

local
  open Parser;

  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  (* For dealing with locations *)

  fun isAlphaNum #"_" = true
    | isAlphaNum c = Char.isAlphaNum c;

  fun locn_to_string (line,char) = 
      "line " ^ Int.toString line ^ ", char " ^ Int.toString char;

  fun some2 p = some (p o snd);

  fun exact2 c = some2 (equal c);

  fun implode2 [] = raise Bug "implode2"
    | implode2 ((x,c) :: rest) = (x, implode (c :: map snd rest));

  (* Adding line numbers *)

  fun read_lines filename =
      Stream.zip (Stream.count 1) (Stream.fromTextFile filename);

  (* Adding char numbers *)

  val char_parser =
      let
        fun f (n:int) (i,c) = ((n,i),c)
      in
        any >> (fn (n,s) => map (f n) (enumerate (explode s)))
      end;

  (* Lexing *)

  fun lex_error l err =
      raise Error ("lexing error at " ^ locn_to_string l ^ ": " ^ err);

  datatype tok = Tnum of int | Tname of string | Tcommand of string;

  val backslash_parser =
      exact2 #"\""
      || exact2 #"\\"
      || (exact2 #"n" >> (I ## K #"\n"))
      || (exact2 #"t" >> (I ## K #"\t"))
      || (any >> (fn (l,c) => lex_error l ("bad char in quote: \\" ^ str c)));

  val quote_parser =
      (exact2 #"\n" >> (fn (l,_) => lex_error l ("newline in quote")))
      || ((exact2 #"\\" ++ backslash_parser) >> snd)
      || some2 (fn c => c <> #"\"" andalso c <> #"\\");

  fun in_comment c = c <> #"\n";

  val tok_parser =
      (atLeastOne (some2 Char.isDigit)
       >> (singleton o (I ## (Tnum o nat_from_string "bad number")) o implode2))
      || ((some2 Char.isAlpha ++ many (some2 isAlphaNum))
          >> (singleton o (I ## Tcommand) o implode2 o op::))
      || ((exact2 #"\"" ++ many quote_parser ++ exact2 #"\"")
          >> (fn ((l,_),(s,_)) => [(l, Tname (implode (map snd s)))]))
      || ((exact2 #"#" ++ many (some2 in_comment) ++ exact2 #"\n") >> (K []))
      || (any >> (fn (l,c) => lex_error l ("bad char: \"" ^ str c ^ "\"")));

  val token_parser =
      ((many (some2 Char.isSpace) ++ tok_parser) >> snd)
      || ((atLeastOne (some2 Char.isSpace) ++ finished) >> K []);

  (* Interpreting commands *)

  type state = {stack : stack, dict : dict, saved : saved, extra : extra};

  fun execute translation cmd {stack,dict,saved,extra} =
      case (cmd,stack) of

      (* Errors *)

        (Tcommand "error", stack) =>
        let
          val stack = Oerror :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Numbers *)

      | (Tnum i, stack) =>
        let
          val stack = Onum i :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Names *)

      | (Tname n, stack) =>
        let
          val stack = Oname n :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Lists *)

      | (Tcommand "nil", stack) =>
        let
          val stack = Olist [] :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "cons", Olist t :: h :: stack) =>
        let
          val stack = Olist (h :: t) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "hd_tl", Olist (h :: t) :: stack) =>
        let
          val stack = Olist t :: h :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Types *)

      | (Tcommand "type_var", Oname n :: stack) =>
        let
          val stack = Otype (mk_type_var n) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "type_op", Olist l :: Oname n :: stack) =>
        let
          val n = import_type translation n
          val (ty,extra) = extra_type_op translation (n, map dest_otype l) extra
          val stack = Otype ty :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
       end

      (* Terms *)

      | (Tcommand "var", Otype ty :: Oname n :: stack) =>
        let
          val stack = Oterm (mk_var (n,ty)) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "const", Otype ty :: Oname n :: stack) =>
        let
          val n = import_const translation n
          val (tm,extra) = extra_const translation (n,ty) extra
          val stack = Oterm tm :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "comb", Oterm b :: Oterm a :: stack) =>
        let
          val stack = Oterm (mk_comb (a,b)) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "abs", Oterm b :: Oterm v :: stack) =>
        let
          val stack = Oterm (mk_abs (dest_var v, b)) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Theorems *)

      | (Tcommand "thm", Oterm c :: Olist h :: stack) =>
        let
          val h = map dest_oterm h
          val (th,extra) = extra_thm translation (h,c) extra
          val stack = Othm th :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Function calls *)

      | (Tcommand "call", Oname n :: i :: stack) =>
        let
          val n = import_rule translation n
(*TRACE1
          val () = if not (null (call_stack stack)) then ()
                   else trace (n ^ "\n")
*)
(*TRACE2
          val () = trace ("  stack = ["^Int.toString (length stack) ^
                          "], call stack = [" ^
                          Int.toString (length (call_stack stack))^"]\n")
          val () = Parser.ppTrace "  input" pp_object i
*)
          val stack = i :: Ocall n :: stack
          val extra = extra_call n i extra
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "return", Oname n :: r :: stack) =>
        let
          val n = import_rule translation n
          val stack =
              case top_call stack of
                NONE => raise Error ("unmatched return "^n)
              | SOME (n',stack) =>
                if n = n' then stack
                else raise Error ("call "^n^" matched by return "^n')
          val stack = r :: stack
(*TRACE1
          val () = if not (null (call_stack stack)) then ()
                   else trace (n ^ " return\n")
*)
(*TRACE2
          val () = trace ("  stack = ["^Int.toString (length stack) ^
                          "], call stack = [" ^
                          Int.toString (length (call_stack stack)) ^ "]\n")
          val () = Parser.ppTrace "return" pp_object r
*)
          val extra = extra_return n r extra
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Dictionary *)

      | (Tcommand "def", Onum n :: d :: stack) =>
        let
          val dict = dict_def (n,d) dict
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "ref", Onum n :: stack) =>
        let
          val obj = dict_ref dict n
          val stack = obj :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* General *)

      | (Tcommand "pop", _ :: stack) =>
        {stack = stack, dict = dict, saved = saved, extra = extra}

      | (Tcommand "dup", stack as x :: _) =>
        let
          val stack = x :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "save", Othm th :: stack) =>
        let
          val saved = saved_add th saved
          val extra = extra_save th extra
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Any other command is an error *)

      | (Tcommand cmd, _) => raise Error ("bad command " ^ cmd);

  fun interpret translation saved extra =
      let
        fun process1 ((l,t),s) =
            execute translation t s
            handle Error err => raise Error ("at "^locn_to_string l^": "^err)

        val state =
            {stack = empty_stack, dict = empty_dict,
             saved = saved, extra = extra}
      in
        Stream.foldl process1 state
      end;

  fun add_textfile (saved,extra) {filename,translation} =
      let
(*TRACE1
        val _ = trace ("filename = " ^ filename ^ "\n")
*)
        val lines = read_lines {filename = filename}
        val chars = everything char_parser lines
        val toks = everything token_parser chars
        val {stack,saved,extra,...} = interpret translation saved extra toks
        val () =
            if null stack then ()
            else warn (plural (length stack) "object" ^ " (including "
                       ^ plural (length (extract_thms stack)) "theorem"
                       ^ ") left on stack at end of\n  " ^ filename)
      in
        (saved,extra)
      end
      handle NoParse => raise Error "parse error";

  fun from_textfile_list l =
      let
        fun f (x,a) = add_textfile a x
        val (saved,extra) = foldl f (saved_empty,extra_init) l
      in
        extra_final saved extra
      end;
in
  fun from_textfile x =
      from_textfile_list [x]
      handle Error err => raise Error ("Article.from_textfile: " ^ err);

  fun from_textfiles xs =
      from_textfile_list xs
      handle Error err => raise Error ("Article.from_textfiles: " ^ err);
end;

(***
local
  fun article_to_stream translation =
      let
        fun line_to_stream s = Stream.singleton (s ^ "\n")
        and num_to_stream n = line_to_stream (Int.toString n)
        and name_to_stream s = line_to_stream ("\"" ^ s ^ "\"")
        and command_to_stream cmd = line_to_stream cmd
        and object_to_stream obj =
            test for dictionary before
            (case obj of
               Oerror => command_to_stream "error"
             | Onum n => num_to_stream n
          | object_to_stream (Oname of s) = name_to_stream s
          | object_to_stream (Olist l) =
            (case l of
               [] => command_to_stream "nil"
             | h :: t =>
               (Stream.flatten o Stream.from_list)
                 [object_to_stream h,
                  object_to_stream (Olist t),
                  command_to_stream "cons"])
          | object_to_stream (Otype ty) =
            (case Ty.dest ty of
               Ty.Type_var n =>
               Stream.append
                 (name_to_stream n)
                 (command_to_stream "type_var")
             | Ty.Type_op (n,l) =
               (Stream.flatten o Stream.from_list)
                 [name_to_stream n,
                  object_to_stream (Olist (map Otype l)),
                  command_to_stream "type_op"])
          | object_to_stream (Oterm of term) = 
          | object_to_stream (Othm of thm) = 
          | object_to_stream (Ocall of name) =
            raise Bug "encountered an Ocall object"
        and call_to_stream name arg =
            (Stream.flatten o Stream.from_list)
              [object_to_stream arg,
               string_to_stream name,
               command_to_stream "call"]
        and return_to_stream name ret =
            (Stream.flatten o Stream.from_list)
              [object_to_stream ret,
               string_to_stream name,
               command_to_stream "return"]
        and op_to_stream (Save_thm th) =
            Stream.append (thm_to_stream th) (command_to_stream "save")
          | op_to_stream (Function_call {name,arg,ops,ret}) =
            (Stream.flatten o Stream.from_list)
              [call_to_stream name arg,
               ops_to_stream ops,
               return_to_stream name ret]
        and ops_to_stream ops =
            Stream.flatten (Stream.map op_to_stream (Stream.from_list ops))
      in
        fn Article ops => ops_to_stream ops
      end;
in
  fun to_textfile {filename, translation, article} =
      let
        val strm = article_to_stream translation article
      in
        Stream.to_textfile {filename = filename} strm
      end
      handle Error err => raise Error ("Article.to_textfile: " ^ err);
end;
***)

end
