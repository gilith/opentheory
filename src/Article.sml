(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful Syntax Rule;

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

fun natFromString err s =
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

val bilingualEmpty = Bilingual {export = NM.new (), import = NM.new ()};

fun bilingualAdd (a,b) (Bilingual {export,import}) =
    let
      fun unseen x m = not (NM.inDomain x m)
      val _ = unseen a export orelse raise Error "export conflict"
      val _ = unseen b import orelse raise Error "import conflict"
      val export = NM.insert export (a,b)
      val import = NM.insert import (b,a)
    in
      Bilingual {export = export, import = import}
    end
    handle Error err => raise Error ("bilingualAdd: " ^ err);

fun bilingualAddl l b = foldl (uncurry bilingualAdd) b l;

fun bilingualExport (Bilingual {export,...}) a =
    Option.getOpt (NM.peek export a, a);

fun bilingualImport (Bilingual {import,...}) b =
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

fun destOerror Oerror = () | destOerror _ = raise Error "destOerror";
val isOerror = can destOerror;

fun destOnum (Onum n) = n | destOnum _ = raise Error "destOnum";
val isOnum = can destOnum;

fun destOname (Oname n) = n | destOname _ = raise Error "destOname";
val isOname = can destOname;

fun destOlist (Olist l) = l | destOlist _ = raise Error "destOlist";
val isOlist = can destOlist;

fun destOtype (Otype ty) = ty | destOtype _ = raise Error "destOtype";
val isOtype = can destOtype;

fun destOterm (Oterm tm) = tm | destOterm _ = raise Error "destOterm";
val isOterm = can destOterm;

fun destOthm (Othm th) = th | destOthm _ = raise Error "destOthm";
val isOthm = can destOthm;

fun destOcall (Ocall n) = n | destOcall _ = raise Error "destOcall";
val isOcall = can destOcall;

fun mkOunit () = Olist [];

fun mkOpair (x,y) = Olist [x,y];
fun destOpair (Olist [x,y]) = (x,y) | destOpair _ = raise Error "destOpair";
val isOpair = can destOpair;

fun destOtriple (Olist [x,y,z]) = (x,y,z)
  | destOtriple _ = raise Error "destOtriple";
val isOtriple = can destOtriple;

val destOvar = (destOname ## destOtype) o destOpair;
val isOvar = can destOvar;

fun objectCompare ob1_ob2 =
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
      | (Othm th1, Othm th2) => Int.compare (thmId th1, thmId th2)
      | (Othm _, _) => LESS
      | (_, Othm _) => GREATER
      | (Olist l1, Olist l2) => lexCompare objectCompare (l1,l2)
      | (Olist _, _) => LESS
      | (_, Olist _) => GREATER
      | (Ocall n1, Ocall n2) => N.compare (n1,n2);

fun ppObject pp ob =
    case ob of
      Oerror => Parser.ppString pp "ERROR"
    | Onum n => Parser.ppInt pp n
    | Oname s => Parser.ppString pp ("\"" ^ s ^ "\"")
    | Otype ty => ppType pp ty
    | Oterm tm => ppTerm pp tm
    | Othm th => ppThm pp th
    | Olist l => Parser.ppList ppObject pp l
    | Ocall f => Parser.ppString pp ("<" ^ f ^ ">");

val extractThms =
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

fun importIntoNamespace namespace name = join "." (namespace @ [name]);

fun exportFromNamespace namespace name =
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
      bilingualImport bilingual (importIntoNamespace namespace name);

  fun export namespace bilingual name =
      exportFromNamespace namespace (bilingualExport bilingual name);
in
  fun importType (Translation {namespace,types,...}) = import namespace types;
  fun exportType (Translation {namespace,types,...}) = export namespace types;

  fun importConst (Translation {namespace = n, consts, ...}) = import n consts;
  fun exportConst (Translation {namespace = n, consts, ...}) = export n consts;

  fun importRule (Translation {namespace,rules,...}) = import namespace rules;
  fun exportRule (Translation {namespace,rules,...}) = export namespace rules;
end;

local
  fun mkTrans l = bilingualAddl l bilingualEmpty;
in
  fun mkTranslation {namespace,types,consts,rules} =
      Translation
        {namespace = namespace,
         types = mkTrans types,
         consts = mkTrans consts,
         rules = mkTrans rules,
         simulations = NM.new ()};
end;

fun addSimulations l translation =
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

fun translationSimulate (Translation {simulations,...}) name =
    case NM.peek simulations name of
      NONE => K NONE
    | SOME sim => SOME o sim;

(* ------------------------------------------------------------------------- *)
(* The natural translation                                                   *)
(* ------------------------------------------------------------------------- *)

val natural =
    mkTranslation
    {namespace = [],
     types = [],
     consts = [],
     rules = []};

(* ------------------------------------------------------------------------- *)
(* Stacks                                                                    *)
(* ------------------------------------------------------------------------- *)

type stack = object list;

val emptyStack : stack = [];

fun topCall [] = NONE
  | topCall (Ocall n :: l) = SOME (n,l)
  | topCall (_ :: l) = topCall l;

val callStack = List.mapPartial (total destOcall);

(* ------------------------------------------------------------------------- *)
(* Dictionaries                                                              *)
(* ------------------------------------------------------------------------- *)

type dict = object IntMap.map;

val emptyDict : dict = IntMap.new ();

fun dictDef (n,obj) dict = IntMap.insert dict (n,obj);

fun dictRef dict n =
    case IntMap.peek dict n of
      SOME obj => obj
    | NONE => raise Error ("dictRef: no entry for number "^Int.toString n);

(* ------------------------------------------------------------------------- *)
(* Saved theorems                                                            *)
(* ------------------------------------------------------------------------- *)

datatype saved = Saved of thm list;

fun savedList (Saved l) = rev l;

val savedEmpty = Saved [];

fun savedAdd th (Saved l) = Saved (th :: l);

(* ------------------------------------------------------------------------- *)
(* Sets of theorems                                                          *)
(* ------------------------------------------------------------------------- *)

datatype thmSet = ThmSet of (term list, thm) Map.map;

local
  fun mkKey (h,c) = c :: TAS.toList h;

  fun thmToKey th = mkKey (hyp th, concl th);

  fun sequentToKey (h,c) = mkKey (TAS.fromList h, c);
in
  val thmSetEmpty = ThmSet (Map.new (lexCompare T.alphaCompare));

  fun thmSetPeek (ThmSet m) h_c = Map.peek m (sequentToKey h_c);

  fun thmSetAdd th (ThmSet m) = ThmSet (Map.insert m (thmToKey th, th));
end;

fun thmSetAddList ths set = foldl (fn (t,s) => thmSetAdd t s) set ths;

(* ------------------------------------------------------------------------- *)
(* The theorem dependency graph                                              *)
(* ------------------------------------------------------------------------- *)

datatype thmDeps = ThmDeps of int list IntMap.map;

val thmDepsEmpty = ThmDeps (IntMap.new ());

fun thmDepsAdd (th,thl) thmDeps =
    let
      val ThmDeps m = thmDeps
      val i = thmId th
      and il = map thmId thl
      val _ = List.all (fn k => k <= i) il orelse
              raise Bug ("thmDepsAdd: bad deps for\n"^thmToString th)
    in
      if mem i il then thmDeps
      else
        let
          val _ = not (Option.isSome (IntMap.peek m i)) orelse
                  raise Bug ("thmDepsAdd: new deps for\n"^thmToString th)
        in
          ThmDeps (IntMap.insert m (i,il))
        end
    end;

fun thmDepsAddList xs deps = foldl (fn (x,d) => thmDepsAdd x d) deps xs;

fun thmDepsUseful (ThmDeps m) =
    let
      fun f [] set = set
        | f (i :: rest) set =
          if IntSet.member i set then f rest set
          else
            case IntMap.peek m i of
              SOME il => f (il @ rest) (IntSet.add set i)
            | NONE => raise Bug ("thmDepsUseful: no dep for "^Int.toString i)
    in
      fn ths => f (map thmId ths) IntSet.empty
    end;

(* ------------------------------------------------------------------------- *)
(* Articles                                                                  *)
(* ------------------------------------------------------------------------- *)

datatype articleOp =
    FunctionCall of
      {name : name,
       arg : object,
       ops : articleOp list,
       ret : object}
  | SaveThm of thm;
         
fun functionCallUpdateOps ops artop =
    case artop of
      FunctionCall {name,arg,ret,...} =>
      FunctionCall {name = name, arg = arg, ops = ops, ret = ret}
    | SaveThm _ => raise Error "functionCallUpdateOps";

datatype article = Article of articleOp list;

val articleEmpty = Article [];

fun articleAppend (Article x) (Article y) = Article (x @ y);

val articleConcat = foldl (fn (x,z) => articleAppend z x) articleEmpty;

val articleOperations =
    let
      fun f n [] = n
        | f n (FunctionCall {ops,...} :: rest) = f (n + 1) (ops @ rest)
        | f n (SaveThm _ :: rest) = f (n + 1) rest
    in
      fn Article l => f 0 l
    end;

fun articleToString article =
    "[" ^ Int.toString (articleOperations article) ^ "]";

val ppArticle = Parser.ppMap articleToString Parser.ppString;

local
  fun specialFilter _ [] s = ([],s)
    | specialFilter p (h :: t) s =
      case p h s of
        SOME (h,s) =>
        let
          val (t,s) = specialFilter p t s
        in
          (h :: t, s)
        end
      | NONE => specialFilter p t s;

  val deleteThms =
      let
        fun f (i,(b,s)) =
            if IntSet.member i s then (true, IntSet.delete s i) else (b,s)
      in
        fn ths => fn set => foldl f (false,set) (map thmId ths)
      end;

  fun eliml ops useful = specialFilter elim ops useful
  and elim (x as SaveThm _) useful = SOME (x,useful)
    | elim (x as FunctionCall {ops,ret,...}) useful =
      let
        val (ops,useful) = eliml ops useful
        val (ok,useful) = deleteThms (extractThms [ret]) useful
      in
        if not (ok orelse not (null ops)) then NONE
        else SOME (functionCallUpdateOps ops x, useful)
      end;
in
  fun deadTheoremElimination useful article = 
    let
(*TRACE1
      val _ = trace ("deadTheoremElimination: before = " ^
                     articleToString article ^ "\n")
*)
      val Article ops = article
      val (ops,useful) = eliml ops useful
      val article = Article ops
(*TRACE1
      val _ = trace ("deadTheoremElimination: after = " ^
                     articleToString article ^ "\n")
*)
    in
      article
    end;
end;

(* ------------------------------------------------------------------------- *)
(* hol-light                                                                 *)
(* ------------------------------------------------------------------------- *)

val holLight =
    ref
      (mkTranslation
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

fun holLightTypeSubstToSubst oins =
    let
      fun f (x,y) = (destTypeVar (destOtype y), destOtype x)
      val l = destOlist oins
    in
      TU.fromListType (map (f o destOpair) l)
    end
    handle Error err =>
      raise Bug ("holLightTypeSubstToSubst failed:\n" ^ err);

fun holLightSubstToSubst oins =
    let
      fun f (x,y) = (destVar (destOterm y), destOterm x)
      val l = destOlist oins
    in
      TU.fromList (map (f o destOpair) l)
    end
    handle Error err =>
      raise Bug ("holLightSubstToSubst failed:\n" ^ err);

fun holLightNewBasicDefinition arg =
    let
      val tm = destOterm arg
      val (v,t) = destEq tm
      val (n,ty) = destVar v
      val n = importConst (!holLight) n
      val v = mkVar (n,ty)
      val tm = mkEq (v,t)
    in
      Othm (Define tm)
    end
    handle Error err =>
      raise Bug ("holLightNewBasicDefinition failed:\n" ^ err);

fun holLightNewBasicTypeDefinition arg =
    let
      val (name,absRep,nonEmptyTh) = destOtriple arg
      val name = importType (!holLight) (destOname name)
      val (abs,rep) = destOpair absRep
      val abs = importConst (!holLight) (destOname abs)
      and rep = importConst (!holLight) (destOname rep)
      and nonEmptyTh = destOthm nonEmptyTh
      val tyVars = NS.toList (T.typeVars (concl nonEmptyTh))
      val (absRepTh,repAbsTh) =
          DefineType name {abs = abs, rep = rep} tyVars nonEmptyTh
    in
      mkOpair (Othm absRepTh, Othm repAbsTh)
    end
    handle Error err =>
      raise Bug ("holLightNewBasicTypeDefinition failed:\n" ^ err);

fun holLightAbs arg =
    let
      val (otm,oth) = destOpair arg
      val v = destVar (destOterm otm)
      val th = destOthm oth
    in
      Othm (Abs v th)
    end;

fun holLightAssume arg = Othm (Assume (destOterm arg));

fun holLightBeta arg = Othm (BetaConv (destOterm arg));

fun holLightDeductAntisymRule arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (DeductAntisym th1 th2)
    end;

fun holLightEqMp arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (EqMp th1 th2)
    end;

fun holLightInst arg =
    let
      val (oins,oth) = destOpair arg
      val ins = holLightSubstToSubst oins
      val th = destOthm oth
    in
      Othm (Subst ins th)
    end;

fun holLightInstType arg =
    let
      val (oins,oth) = destOpair arg
      val ins = holLightTypeSubstToSubst oins
      val th = destOthm oth
    in
      Othm (Subst ins th)
    end;

fun holLightMkComb arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (MkComb th1 th2)
    end;

fun holLightRefl arg = Othm (Refl (destOterm arg));

fun holLightTrans arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (Trans th1 th2)
    end;

val () =
    holLight :=
    addSimulations
      [("hol-light.newBasicDefinition", holLightNewBasicDefinition),
       ("hol-light.newBasicTypeDefinition", holLightNewBasicTypeDefinition),
       ("hol-light.ABS", holLightAbs),
       ("hol-light.ASSUME", holLightAssume),
       ("hol-light.BETA", holLightBeta),
       ("hol-light.DEDUCT_ANTISYM_RULE", holLightDeductAntisymRule),
       ("hol-light.EQ_MP", holLightEqMp),
       ("hol-light.INST", holLightInst),
       ("hol-light.INST_TYPE", holLightInstType),
       ("hol-light.MK_COMB", holLightMkComb),
       ("hol-light.REFL", holLightRefl),
       ("hol-light.TRANS", holLightTrans)]
      (!holLight);

(* ------------------------------------------------------------------------- *)
(* Getting hold of required theorems by hook or by crook                     *)
(* ------------------------------------------------------------------------- *)

datatype particle =
         Particle of {name : name,
                      arg : object,
                      revOps : articleOp list,
                      thms : thmSet};

datatype extra =
         Extra of {particles : particle * particle list,
                   newTypes : NS.set,
                   newConsts : NS.set,
                   newAxioms : thm list,
                   savedThms : thmSet,
                   thmDeps : thmDeps};

fun particleName (Particle {name,...}) = name;

fun particleArg (Particle {arg,...}) = arg;

fun particleRevOps (Particle {revOps,...}) = revOps;

fun particleThms (Particle {thms,...}) = thms;

fun particleUpdateName name particle =
    let
      val Particle {arg,revOps,thms,...} = particle
    in
      Particle {name = name, arg = arg, revOps = revOps, thms = thms}
    end;

fun particleUpdateArg arg particle =
    let
      val Particle {name,revOps,thms,...} = particle
    in
      Particle {name = name, arg = arg, revOps = revOps, thms = thms}
    end;

fun particleUpdateRevOps revOps particle =
    let
      val Particle {name,arg,thms,...} = particle
    in
      Particle {name = name, arg = arg, revOps = revOps, thms = thms}
    end;

fun particleUpdateThms thms particle =
    let
      val Particle {name,arg,revOps,...} = particle
    in
      Particle {name = name, arg = arg, revOps = revOps, thms = thms}
    end;

fun mkParticle name arg thms =
    Particle {name = name, arg = arg, revOps = [], thms = thms};

fun particleOps particle = rev (particleRevOps particle);

fun particleAddOp op' particle =
    particleUpdateRevOps (op' :: particleRevOps particle) particle;

fun extraParticles (Extra {particles,...}) = particles;

fun extraNewTypes (Extra {newTypes,...}) = newTypes;

fun extraNewConsts (Extra {newConsts,...}) = newConsts;

fun extraNewAxioms (Extra {newAxioms,...}) = newAxioms;

fun extraSavedThms (Extra {savedThms,...}) = savedThms;

fun extraThmDeps (Extra {thmDeps,...}) = thmDeps;

fun extraUpdateParticles particles extra =
    let
      val Extra {newTypes, newConsts, newAxioms,
                 savedThms, thmDeps, ...} = extra
    in
      Extra
        {particles = particles, newTypes = newTypes,
         newConsts = newConsts, newAxioms = newAxioms,
         savedThms = savedThms, thmDeps = thmDeps}
    end;

fun extraUpdateNewTypes newTypes extra =
    let
      val Extra {particles, newConsts, newAxioms,
                 savedThms, thmDeps, ...} = extra
    in
      Extra
        {particles = particles, newTypes = newTypes,
         newConsts = newConsts, newAxioms = newAxioms,
         savedThms = savedThms, thmDeps = thmDeps}
    end;

fun extraUpdateNewConsts newConsts extra =
    let
      val Extra {particles, newTypes, newAxioms,
                 savedThms, thmDeps, ...} = extra
    in
      Extra
        {particles = particles, newTypes = newTypes,
         newConsts = newConsts, newAxioms = newAxioms,
         savedThms = savedThms, thmDeps = thmDeps}
    end;

fun extraUpdateNewAxioms newAxioms extra =
    let
      val Extra {particles, newTypes, newConsts,
                 savedThms, thmDeps, ...} = extra
    in
      Extra
        {particles = particles, newTypes = newTypes,
         newConsts = newConsts, newAxioms = newAxioms,
         savedThms = savedThms, thmDeps = thmDeps}
    end;

fun extraUpdateSavedThms savedThms extra =
    let
      val Extra {particles, newTypes, newConsts,
                 newAxioms, thmDeps, ...} = extra
    in
      Extra
        {particles = particles, newTypes = newTypes,
         newConsts = newConsts, newAxioms = newAxioms,
         savedThms = savedThms, thmDeps = thmDeps}
    end;

fun extraUpdateThmDeps thmDeps extra =
    let
      val Extra {particles, newTypes, newConsts,
                 newAxioms, savedThms, ...} = extra
    in
      Extra
        {particles = particles, newTypes = newTypes,
         newConsts = newConsts, newAxioms = newAxioms,
         savedThms = savedThms, thmDeps = thmDeps}
    end;

fun extraName extra =
    let
      val (topParticle,_) = extraParticles extra
    in
      particleName topParticle
    end;

fun extraArg extra =
    let
      val (topParticle,_) = extraParticles extra
    in
      particleArg topParticle
    end;

fun extraThms extra =
    let
      val (topParticle,_) = extraParticles extra
    in
      particleThms topParticle
    end;

fun extraUpdateThms thms extra =
    let
      val (topParticle,particles) = extraParticles extra
      val topParticle = particleUpdateThms thms topParticle
    in
      extraUpdateParticles (topParticle,particles) extra
    end;

fun extraThmsAddList thl extra =
    let
      val thms = extraThms extra
      val thms = thmSetAddList thl thms
    in
      extraUpdateThms thms extra
    end;

fun extraAddNewType n extra =
    let
      val newTypes = extraNewTypes extra
      val newTypes = NS.add newTypes n
    in
      extraUpdateNewTypes newTypes extra
    end;

fun extraAddNewConst n extra =
    let
      val newConsts = extraNewConsts extra
      val newConsts = NS.add newConsts n
    in
      extraUpdateNewConsts newConsts extra
    end;

fun extraAddNewAxiom th extra =
    let
      val newAxioms = extraNewAxioms extra
      val newAxioms = th :: newAxioms
    in
      extraUpdateNewAxioms newAxioms extra
    end;

fun extraAddOp op' extra =
    let
      val (topParticle,particles) = extraParticles extra
      val topParticle = particleAddOp op' topParticle
    in
      extraUpdateParticles (topParticle,particles) extra
    end;

fun extraThmDepsAdd deps extra =
    let
      val thmDeps = extraThmDeps extra
      val thmDeps = thmDepsAdd deps thmDeps
    in
      extraUpdateThmDeps thmDeps extra
    end;

fun extraThmDepsAddList deps extra =
    let
      val thmDeps = extraThmDeps extra
      val thmDeps = thmDepsAddList deps thmDeps
    in
      extraUpdateThmDeps thmDeps extra
    end;

val ppExtra = Parser.ppMap (fn _ : extra => "<extra>") Parser.ppString;

val extraInit =
    Extra
      {particles = (mkParticle "<main>" (mkOunit ()) thmSetEmpty, []),
       newTypes = NS.empty,
       newConsts = NS.empty,
       newAxioms = [],
       savedThms = thmSetEmpty,
       thmDeps = thmDepsEmpty};
    
fun extraFindTheorem ths hC extra =
    case thmSetPeek ths hC of
      NONE => NONE
    | SOME th =>
      let
        val th' = Alpha hC th
        val extra = extraThmDepsAdd (th',[th]) extra
      in
        SOME (th',extra)
      end;

fun extraSimulate translation extra =
    let
      val arg = extraArg extra
    in
      case translationSimulate translation (extraName extra) arg of
        NONE => extra
      | SOME result =>
        let
          val resultThl = extractThms [result]
          val extra = extraThmsAddList resultThl extra
          val argThl = extractThms [arg]
          val newDeps = map (fn th => (th,argThl)) resultThl
          val extra = extraThmDepsAddList newDeps extra
        in
          extra
        end
    end;

local
  val typeOpExists = can Ty.typeArity;

  fun extraNewTypeOp (n,arity) extra =
      let
        val () = warn ("making new type operator " ^ n)
        val () = Ty.declareType n arity
      in
        extraAddNewType n extra
      end;
in
  fun extraTypeOp translation (n,l) extra =
      let
        val extra =
            if typeOpExists n then extra
            else
              let
                val extra = extraSimulate translation extra
              in
                if typeOpExists n then extra
                else extraNewTypeOp (n, length l) extra
              end
      in
        (mkTypeOp (n,l), extra)
      end;
end;

local
  val constExists = can T.constType;

  fun extraNewConst n extra =
      let
        val () = warn ("making new constant " ^ n)
        val () = T.declareConst n alpha
      in
        extraAddNewConst n extra
      end;
in
  fun extraConst translation (n,ty) extra =
      let
        val extra =
            if constExists n then extra
            else
              let
                val extra = extraSimulate translation extra
              in
                if constExists n then extra
                else extraNewConst n extra
              end
      in
        (mkConst (n,ty), extra)
      end;
end;

fun extraAxiom (h,c) extra =
    let
      val th = Axiom {hyp = TAS.fromList h, concl = c}
      val () = warn ("making new axiom:\n" ^ thmToString th)
      val extra = extraAddNewAxiom th extra
      val extra = extraThmDepsAdd (th,[]) extra
    in
      (th,extra)
    end;

fun extraThm translation hC extra =
    case extraFindTheorem (extraSavedThms extra) hC extra of
      SOME thExtra => thExtra
    | NONE =>
      case extraFindTheorem (extraThms extra) hC extra of
        SOME thExtra => thExtra
      | NONE =>
        let
          val extra = extraSimulate translation extra
        in
          case extraFindTheorem (extraThms extra) hC extra of
            SOME thExtra => thExtra
          | NONE => extraAxiom hC extra
        end;

fun extraCall name arg extra =
    let
      val (topParticle,particles) = extraParticles extra
      val newParticle = mkParticle name arg (particleThms topParticle)
    in
      extraUpdateParticles (newParticle, topParticle :: particles) extra
    end;

fun extraReturn name ret extra =
    let
      val (topParticle,particles) = extraParticles extra
      val (nextParticle,particles) =
          case particles of
            [] => raise Bug "extraReturn: bad particle list"
          | next :: rest => (next,rest)
      val newFunctionCall =
          let
            val _ = name = particleName topParticle orelse
                    raise Bug "extraReturn: name mismatch"
            val arg = particleArg topParticle
            and ops = particleOps topParticle
          in
            FunctionCall {name = name, arg = arg, ops = ops, ret = ret}
          end
      val extra = extraUpdateParticles (nextParticle,particles) extra
      val extra = extraAddOp newFunctionCall extra
      val thms = thmSetAddList (extractThms [ret]) (extraThms extra)
      val extra = extraUpdateThms thms extra
    in
      extra
    end;

fun extraSave th extra = extraAddOp (SaveThm th) extra;

fun extraFinal saved extra =
    let
      val particle =
          case extraParticles extra of
            (topParticle,[]) => topParticle
          | (_, _ :: _) => raise Error "article ended inside a function call"
      val _ = particleName particle = "<main>" orelse
              raise Bug "extraFinal: bad particle name"
      val article = Article (particleOps particle)
      val saved = savedList saved
      val useful = thmDepsUseful (extraThmDeps extra) saved
      val article = deadTheoremElimination useful article
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

  fun locnToString (line,char) = 
      "line " ^ Int.toString line ^ ", char " ^ Int.toString char;

  fun some2 p = some (p o snd);

  fun exact2 c = some2 (equal c);

  fun implode2 [] = raise Bug "implode2"
    | implode2 ((x,c) :: rest) = (x, implode (c :: map snd rest));

  (* Adding line numbers *)

  fun readLines filename =
      Stream.zip (Stream.count 1) (Stream.fromTextFile filename);

  (* Adding char numbers *)

  val charParser =
      let
        fun f (n:int) (i,c) = ((n,i),c)
      in
        any >> (fn (n,s) => map (f n) (enumerate (explode s)))
      end;

  (* Lexing *)

  fun lexError l err =
      raise Error ("lexing error at " ^ locnToString l ^ ": " ^ err);

  datatype tok = Tnum of int | Tname of string | Tcommand of string;

  val backslashParser =
      exact2 #"\""
      || exact2 #"\\"
      || (exact2 #"n" >> (I ## K #"\n"))
      || (exact2 #"t" >> (I ## K #"\t"))
      || (any >> (fn (l,c) => lexError l ("bad char in quote: \\" ^ str c)));

  val quoteParser =
      (exact2 #"\n" >> (fn (l,_) => lexError l ("newline in quote")))
      || ((exact2 #"\\" ++ backslashParser) >> snd)
      || some2 (fn c => c <> #"\"" andalso c <> #"\\");

  fun inComment c = c <> #"\n";

  val tokParser =
      (atLeastOne (some2 Char.isDigit)
       >> (singleton o (I ## (Tnum o natFromString "bad number")) o implode2))
      || ((some2 Char.isAlpha ++ many (some2 isAlphaNum))
          >> (singleton o (I ## Tcommand) o implode2 o op::))
      || ((exact2 #"\"" ++ many quoteParser ++ exact2 #"\"")
          >> (fn ((l,_),(s,_)) => [(l, Tname (implode (map snd s)))]))
      || ((exact2 #"#" ++ many (some2 inComment) ++ exact2 #"\n") >> (K []))
      || (any >> (fn (l,c) => lexError l ("bad char: \"" ^ str c ^ "\"")));

  val tokenParser =
      ((many (some2 Char.isSpace) ++ tokParser) >> snd)
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

      | (Tcommand "hdTl", Olist (h :: t) :: stack) =>
        let
          val stack = Olist t :: h :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Types *)

      | (Tcommand "typeVar", Oname n :: stack) =>
        let
          val stack = Otype (mkTypeVar n) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "typeOp", Olist l :: Oname n :: stack) =>
        let
          val n = importType translation n
          val (ty,extra) = extraTypeOp translation (n, map destOtype l) extra
          val stack = Otype ty :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
       end

      (* Terms *)

      | (Tcommand "var", Otype ty :: Oname n :: stack) =>
        let
          val stack = Oterm (mkVar (n,ty)) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "const", Otype ty :: Oname n :: stack) =>
        let
          val n = importConst translation n
          val (tm,extra) = extraConst translation (n,ty) extra
          val stack = Oterm tm :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "comb", Oterm b :: Oterm a :: stack) =>
        let
          val stack = Oterm (mkComb (a,b)) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "abs", Oterm b :: Oterm v :: stack) =>
        let
          val stack = Oterm (mkAbs (destVar v, b)) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Theorems *)

      | (Tcommand "thm", Oterm c :: Olist h :: stack) =>
        let
          val h = map destOterm h
          val (th,extra) = extraThm translation (h,c) extra
          val stack = Othm th :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Function calls *)

      | (Tcommand "call", Oname n :: i :: stack) =>
        let
          val n = importRule translation n
(*TRACE1
          val () = if not (null (callStack stack)) then ()
                   else trace (n ^ "\n")
*)
(*TRACE2
          val () = trace ("  stack = ["^Int.toString (length stack) ^
                          "], call stack = [" ^
                          Int.toString (length (callStack stack))^"]\n")
          val () = Parser.ppTrace "  input" ppObject i
*)
          val stack = i :: Ocall n :: stack
          val extra = extraCall n i extra
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "return", Oname n :: r :: stack) =>
        let
          val n = importRule translation n
          val stack =
              case topCall stack of
                NONE => raise Error ("unmatched return "^n)
              | SOME (n',stack) =>
                if n = n' then stack
                else raise Error ("call "^n^" matched by return "^n')
          val stack = r :: stack
(*TRACE1
          val () = if not (null (callStack stack)) then ()
                   else trace (n ^ " return\n")
*)
(*TRACE2
          val () = trace ("  stack = ["^Int.toString (length stack) ^
                          "], call stack = [" ^
                          Int.toString (length (callStack stack)) ^ "]\n")
          val () = Parser.ppTrace "return" ppObject r
*)
          val extra = extraReturn n r extra
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Dictionary *)

      | (Tcommand "def", Onum n :: d :: stack) =>
        let
          val dict = dictDef (n,d) dict
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "ref", Onum n :: stack) =>
        let
          val obj = dictRef dict n
          val stack = obj :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* General *)

      | (Tcommand "pop", _ :: stack) =>
        {stack = stack, dict = dict, saved = saved, extra = extra}

      | (Tcommand "dup", Onum n :: stack) =>
        let
          val _ = length stack > n orelse raise Error "bad dup"
          val stack = List.nth (stack,n) :: stack
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      | (Tcommand "save", Othm th :: stack) =>
        let
          val saved = savedAdd th saved
          val extra = extraSave th extra
        in
          {stack = stack, dict = dict, saved = saved, extra = extra}
        end

      (* Any other command is an error *)

      | (Tcommand cmd, _) => raise Error ("bad command " ^ cmd);

  fun interpret translation saved extra =
      let
        fun process1 ((l,t),s) =
            execute translation t s
            handle Error err => raise Error ("at "^locnToString l^": "^err)

        val state =
            {stack = emptyStack, dict = emptyDict,
             saved = saved, extra = extra}
      in
        Stream.foldl process1 state
      end;

  fun addTextFile (saved,extra) {filename,translation} =
      let
(*TRACE1
        val _ = trace ("filename = " ^ filename ^ "\n")
*)
        val lines = readLines {filename = filename}
        val chars = everything charParser lines
        val toks = everything tokenParser chars
        val {stack,saved,extra,...} = interpret translation saved extra toks
        val () =
            if null stack then ()
            else warn (plural (length stack) "object" ^ " (including "
                       ^ plural (length (extractThms stack)) "theorem"
                       ^ ") left on stack at end of\n  " ^ filename)
      in
        (saved,extra)
      end
      handle NoParse => raise Error "parse error";

  fun fromTextFileList l =
      let
        fun f (x,a) = addTextFile a x
        val (saved,extra) = foldl f (savedEmpty,extraInit) l
      in
        extraFinal saved extra
      end;
in
  fun fromTextFile x =
      fromTextFileList [x]
      handle Error err => raise Error ("Article.fromTextFile: " ^ err);

  fun fromTextFiles xs =
      fromTextFileList xs
      handle Error err => raise Error ("Article.fromTextFiles: " ^ err);
end;

(***
local
  fun articleToStream translation =
      let
        fun lineToStream s = Stream.singleton (s ^ "\n")
        and numToStream n = lineToStream (Int.toString n)
        and nameToStream s = lineToStream ("\"" ^ s ^ "\"")
        and commandToStream cmd = lineToStream cmd
        and objectToStream obj =
            test for dictionary before
            (case obj of
               Oerror => commandToStream "error"
             | Onum n => numToStream n
          | objectToStream (Oname of s) = nameToStream s
          | objectToStream (Olist l) =
            (case l of
               [] => commandToStream "nil"
             | h :: t =>
               (Stream.flatten o Stream.fromList)
                 [objectToStream h,
                  objectToStream (Olist t),
                  commandToStream "cons"])
          | objectToStream (Otype ty) =
            (case Ty.dest ty of
               Ty.TypeVar n =>
               Stream.append
                 (nameToStream n)
                 (commandToStream "typeVar")
             | Ty.TypeOp (n,l) =
               (Stream.flatten o Stream.fromList)
                 [nameToStream n,
                  objectToStream (Olist (map Otype l)),
                  commandToStream "typeOp"])
          | objectToStream (Oterm of term) = 
          | objectToStream (Othm of thm) = 
          | objectToStream (Ocall of name) =
            raise Bug "encountered an Ocall object"
        and callToStream name arg =
            (Stream.flatten o Stream.fromList)
              [objectToStream arg,
               stringToStream name,
               commandToStream "call"]
        and returnToStream name ret =
            (Stream.flatten o Stream.fromList)
              [objectToStream ret,
               stringToStream name,
               commandToStream "return"]
        and opToStream (SaveThm th) =
            Stream.append (thmToStream th) (commandToStream "save")
          | opToStream (FunctionCall {name,arg,ops,ret}) =
            (Stream.flatten o Stream.fromList)
              [callToStream name arg,
               opsToStream ops,
               returnToStream name ret]
        and opsToStream ops =
            Stream.flatten (Stream.map opToStream (Stream.fromList ops))
      in
        fn Article ops => opsToStream ops
      end;
in
  fun toTextFile {filename, translation, article} =
      let
        val strm = articleToStream translation article
      in
        Stream.toTextFile {filename = filename} strm
      end
      handle Error err => raise Error ("Article.toTextFile: " ^ err);
end;
***)

end
