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

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun plural 1 s = "1 " ^ s
  | plural n s = Int.toString n ^ " " ^ s ^ "s";

fun natFromString err s =
    case Int.fromString s of
      SOME i => i
    | NONE => raise Error err;

(* ------------------------------------------------------------------------- *)
(* Namespaces.                                                               *)
(* ------------------------------------------------------------------------- *)

type namespace = name list;

val globalNamespace : namespace = [];

fun exportFromNamespace namespace name = join "." (namespace @ [name]);

local
  fun import (m,x) = Option.getOpt (total (destPrefix (m ^ ".")) x, x);
in
  fun importIntoNamespace namespace name = foldl import name namespace;
end;

(* ------------------------------------------------------------------------- *)
(* Bilingual dictionaries.                                                   *)
(* ------------------------------------------------------------------------- *)

datatype bilingual =
    Bilingual of
      {export : name NM.map,
       import : name NM.map};

val bilingualEmpty = Bilingual {export = NM.new (), import = NM.new ()};

local
  fun unseen x m = not (NM.inDomain x m);
in
  fun bilingualAdd namespace bilingual (localName,altName) =
      let
        val Bilingual {export,import} = bilingual
        val globalName = exportFromNamespace namespace localName
        val _ = unseen globalName export orelse raise Error "export conflict"
        val _ = unseen altName import orelse raise Error "import conflict"
        val export = NM.insert export (globalName,altName)
        val import = NM.insert import (altName,globalName)
      in
        Bilingual {export = export, import = import}
      end
      handle Error err => raise Error ("bilingualAdd: " ^ err);
end;

fun bilingualAddList namespace =
    let
      fun add (entry,bilingual) = bilingualAdd namespace bilingual entry
    in
      List.foldl add
    end;

fun bilingualFromList namespace = bilingualAddList namespace bilingualEmpty;

fun bilingualExport namespace (Bilingual {export,...}) localName =
    let
      val globalName = exportFromNamespace namespace localName
    in
      case NM.peek export globalName of
        SOME altName => altName
      | NONE => globalName
    end;

fun bilingualImport namespace (Bilingual {import,...}) altName =
    let
      val globalName =
          case NM.peek import altName of
            SOME globalName => globalName
          | NONE => altName
    in
      importIntoNamespace namespace globalName
    end;

(* ------------------------------------------------------------------------- *)
(* Objects.                                                                  *)
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

fun destOerror Oerror = ()
  | destOerror _ = raise Error "destOerror";
val isOerror = can destOerror;

fun destOnum (Onum n) = n
  | destOnum _ = raise Error "destOnum";
val isOnum = can destOnum;

fun destOname (Oname n) = n
  | destOname _ = raise Error "destOname";
val isOname = can destOname;

fun destOlist (Olist l) = l
  | destOlist _ = raise Error "destOlist";
val isOlist = can destOlist;

fun destOtype (Otype ty) = ty
  | destOtype _ = raise Error "destOtype";
val isOtype = can destOtype;

fun destOterm (Oterm tm) = tm
  | destOterm _ = raise Error "destOterm";
val isOterm = can destOterm;

fun destOthm (Othm th) = th
  | destOthm _ = raise Error "destOthm";
val isOthm = can destOthm;

fun destOcall (Ocall n) = n
  | destOcall _ = raise Error "destOcall";
val isOcall = can destOcall;

fun mkOunit () = Olist [];

fun mkOpair (x,y) = Olist [x,y];
fun destOpair (Olist [x,y]) = (x,y)
  | destOpair _ = raise Error "destOpair";
val isOpair = can destOpair;

fun destOtriple (Olist [x,y,z]) = (x,y,z)
  | destOtriple _ = raise Error "destOtriple";
val isOtriple = can destOtriple;

fun destOvar var =
    let
      val (name,ty) = destOpair var
    in
      (destOname name, destOtype ty)
    end;
val isOvar = can destOvar;

fun objectCompare ob1_ob2 =
    if Portable.pointerEqual ob1_ob2 then EQUAL
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
      | (Othm th1, Othm th2) => Thm.compare (th1,th2)
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

val objectThms =
    let
      fun f acc [] = acc
        | f acc (Othm th :: rest) = f (th :: acc) rest
        | f acc (Olist l :: rest) = f acc (l @ rest)
        | f acc (_ :: rest) = f acc rest
    in
      fn obj => f [] [obj]
    end;

(* ------------------------------------------------------------------------- *)
(* Translations.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype translation =
    Translation of
      {namespace : namespace,
       types : bilingual,
       consts : bilingual,
       rules : (object -> object) NM.map};

fun exportType (Translation {namespace,types,...}) =
    bilingualExport namespace types;

fun importType (Translation {namespace,types,...}) =
    bilingualImport namespace types;

fun exportConst (Translation {namespace,consts,...}) =
    bilingualExport namespace consts;

fun importConst (Translation {namespace,consts,...}) =
    bilingualImport namespace consts;

fun addRule (Translation {namespace,types,consts,rules}) (name,rule) =
    let
      val name = exportFromNamespace namespace name
      val _ = not (NM.inDomain name rules) orelse
              raise Bug ("duplicate rule: " ^ name)
      val rules = NM.insert rules (name,rule)
    in
      Translation
        {namespace = namespace,
         types = types,
         consts = consts,
         rules = rules}
    end;

val addRuleList =
    let
      fun add (name_rule,trans) = addRule trans name_rule
    in
      List.foldl add
    end;
    
fun mkTranslation {namespace,types,consts,rules} =
    let
      val trans =
          Translation
            {namespace = namespace,
             types = bilingualFromList namespace types,
             consts = bilingualFromList namespace consts,
             rules = NM.new ()}
    in
      addRuleList trans rules
    end;
  
(***
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
***)

(* ------------------------------------------------------------------------- *)
(* The natural translation.                                                  *)
(* ------------------------------------------------------------------------- *)

val natural =
    mkTranslation
      {namespace = globalNamespace,
       types = [],
       consts = [],
       rules = []};

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type objectId = int;

val newObjectId : unit -> objectId =
    let
      val counter = ref 0
    in
      fn () =>
         let
           val ref count = counter
           val () = counter := count + 1
         in
           count
         end
    end;

(* ------------------------------------------------------------------------- *)
(* Object provenance.                                                        *)
(*                                                                           *)
(* Invariants (in order of priority):                                        *)
(*                                                                           *)
(* 1. The callObject slot contains Ocall callName objects                    *)
(*    that have provenance Pcall callArgument.                               *)
(*                                                                           *)
(* 2. Objects that do not contain theorems have provenance Pnull             *)
(*    (because they can be easily constructed).                              *)
(*                                                                           *)
(* 3. The copy of the call argument has provenance PcallArgument.            *)
(* ------------------------------------------------------------------------- *)

datatype objectProvenance =
    Op of
      {objectId : objectId,
       object : object,
       provenance : provenance,
       callObject : objectProvenance option}

and provenance =
    Pnull
  | Pcons of objectProvenance * objectProvenance
  | Pthm of objectProvenance list
  | Pref of objectProvenance
  | Pcall of objectProvenance
  | PcallArgument
  | Preturn of objectProvenance;

fun object (Op {object = x, ...}) = x;

fun destOpthm obj = destOthm (object obj);

fun destOpcall obj = destOcall (object obj);

fun opThms obj = objectThms (object obj);

(* ------------------------------------------------------------------------- *)
(* Theorems in scope.                                                        *)
(* ------------------------------------------------------------------------- *)

datatype theorems =
    Theorems of (thm * objectProvenance list) SequentMap.map;

val noTheorems = Theorems (SequentMap.new ());

fun addTheorems thms th_prov =
    let
      val Theorems thmsMap = thms
      and (th,_) = th_prov
      val seq = sequent th
      val block =
          case SequentMap.peek thmsMap seq of
            NONE => false
          | SOME (_,[]) => true
          | SOME (_, _ :: _) => false
    in
      if block then thms
      else Theorems (SequentMap.insert thmsMap (seq,th_prov))
    end;

local
  fun add prov (th,thms) = addTheorems thms (th,prov);
in
  fun addOpTheorems thms obj = List.foldl (add [obj]) thms (opThms obj);

  fun addNoOpTheorems thms obj = List.foldl (add []) thms (opThms obj);
end;

(* ------------------------------------------------------------------------- *)
(* Stacks.                                                                   *)
(* ------------------------------------------------------------------------- *)

type stack = (objectProvenance * theorems) list;

val emptyStack : stack = [];

fun pushIdStack (object,provenance,callObject) stack : stack =
    let
      val objectId = newObjectId ()
      val obj =
          Op {objectId = objectId,
              object = object,
              provenance = provenance,
              callObject = callObject}
      val thms =
          case stack of
            [] => noTheorems
          | (_,thms) :: _ => thms
      val thms = addOpTheorems thms obj
    in
      (obj,thms) :: stack
    end;

fun pushStack (object,provenance) stack =
    let
      val callObject =
          case stack of
            [] => NONE
          | (Op {callObject,...}, _) :: _ => callObject
    in
      pushIdStack (object,provenance,callObject) stack
    end;

fun topCall [] = NONE
  | topCall ((Op {object = Ocall n, ...}, _) :: l) = SOME (n,l)
  | topCall (_ :: l) = topCall l;

(* ------------------------------------------------------------------------- *)
(* Call stacks.                                                              *)
(* ------------------------------------------------------------------------- *)

type callStack = (name * object) list;

fun callStack (Op {object = Ocall name, callObject, provenance, ...}) =
    (case provenance of
       Pcall (Op {object,...}) => (name,object) :: callObjectStack callObject
     | _ => raise Bug "Ocall object does not have Pcall name provenance")
  | callStack (Op {callObject,...}) = callObjectStack callObject

and callObjectStack NONE = []
  | callObjectStack (SOME obj) = callStack obj;

(* ------------------------------------------------------------------------- *)
(* Dictionaries.                                                             *)
(* ------------------------------------------------------------------------- *)

type dict = objectProvenance IntMap.map;

val emptyDict : dict = IntMap.new ();

fun dictDef (n,obj) dict = IntMap.insert dict (n,obj);

fun dictRef dict n =
    case IntMap.peek dict n of
      SOME obj => obj
    | NONE => raise Error ("dictRef: no entry for number "^Int.toString n);

(* ------------------------------------------------------------------------- *)
(* Saved theorems.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype saved = Saved of objectProvenance list;

fun savedList (Saved objs) = rev objs;

val savedEmpty = Saved [];

fun savedAdd obj (Saved objs) = Saved (obj :: objs);

(***
(* ------------------------------------------------------------------------- *)
(* Sets of theorems.                                                         *)
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
(* The theorem dependency graph.                                             *)
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
***)

(* ------------------------------------------------------------------------- *)
(* hol-light                                                                 *)
(* ------------------------------------------------------------------------- *)

val holLight =
    mkTranslation
      {namespace = ["hol-light"],
       types = [("fun","fun"),
                ("bool","bool")],
       consts = [("=","="),
                 ("@","@"),
                 ("T","T"),
                 ("F","F"),
                 ("~","~"),
                 ("/\\","/\\"),
                 ("\\/","\\/"),
                 ("==>","==>"),
                 ("!","!"),
                 ("?","?"),
                 ("?!","?!")],
       rules = []};

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
      val n = importConst holLight n
      val v = mkVar (n,ty)
      val tm = mkEq (v,t)
    in
      Othm (define tm)
    end
    handle Error err =>
      raise Bug ("holLightNewBasicDefinition failed:\n" ^ err);

fun holLightNewBasicTypeDefinition arg =
    let
      val (name,absRep,nonEmptyTh) = destOtriple arg
      val name = importType holLight (destOname name)
      val (abs,rep) = destOpair absRep
      val abs = importConst holLight (destOname abs)
      and rep = importConst holLight (destOname rep)
      and nonEmptyTh = destOthm nonEmptyTh
      val tyVars = NS.toList (T.typeVars (concl nonEmptyTh))
      val (absRepTh,repAbsTh) =
          defineType name {abs = abs, rep = rep} tyVars nonEmptyTh
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
      Othm (abs v th)
    end;

fun holLightAssume arg = Othm (assume (destOterm arg));

fun holLightBeta arg = Othm (betaConv (destOterm arg));

fun holLightDeductAntisymRule arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (deductAntisym th1 th2)
    end;

fun holLightEqMp arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (eqMp th1 th2)
    end;

fun holLightInst arg =
    let
      val (oins,oth) = destOpair arg
      val ins = holLightSubstToSubst oins
      val th = destOthm oth
    in
      Othm (subst ins th)
    end;

fun holLightInstType arg =
    let
      val (oins,oth) = destOpair arg
      val ins = holLightTypeSubstToSubst oins
      val th = destOthm oth
    in
      Othm (subst ins th)
    end;

fun holLightMkComb arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (comb th1 th2)
    end;

fun holLightRefl arg = Othm (refl (destOterm arg));

fun holLightTrans arg =
    let
      val (oth1,oth2) = destOpair arg
      val th1 = destOthm oth1
      val th2 = destOthm oth2
    in
      Othm (trans th1 th2)
    end;

val holLight =
    addRuleList holLight
      [("newBasicDefinition", holLightNewBasicDefinition),
       ("newBasicTypeDefinition", holLightNewBasicTypeDefinition),
       ("ABS", holLightAbs),
       ("ASSUME", holLightAssume),
       ("BETA", holLightBeta),
       ("DEDUCT_ANTISYM_RULE", holLightDeductAntisymRule),
       ("EQ_MP", holLightEqMp),
       ("INST", holLightInst),
       ("INST_TYPE", holLightInstType),
       ("MK_COMB", holLightMkComb),
       ("REFL", holLightRefl),
       ("TRANS", holLightTrans)];

(***
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
***)

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

  fun map2 f (l,x) = (l, f x);

  fun k2 x (l,_) = (l,x);

  fun implode2 [] = raise Bug "implode2: empty"
    | implode2 ((l,c) :: rest) = (l, implode (c :: map snd rest));

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

  fun tokToString tok =
      case tok of
        Tnum i => Int.toString i
      | Tname s => "\"" ^ s ^ "\""
      | Tcommand s => s;

  val backslashParser =
      exact2 #"\""
      || exact2 #"\\"
      || (exact2 #"n" >> k2 #"\n")
      || (exact2 #"t" >> k2 #"\t")
      || (any >> (fn (l,c) => lexError l ("bad char in quote: \\" ^ str c)));

  val quoteParser =
      (exact2 #"\n" >> (fn (l,_) => lexError l ("newline in quote")))
      || ((exact2 #"\\" ++ backslashParser) >> snd)
      || some2 (fn c => c <> #"\"" andalso c <> #"\\");

  fun inComment c = c <> #"\n";

  val tokParser =
      (atLeastOne (some2 Char.isDigit)
       >> (singleton o (map2 (Tnum o natFromString "bad number")) o implode2))
      || ((some2 Char.isAlpha ++ many (some2 isAlphaNum))
          >> (singleton o map2 Tcommand o implode2 o op::))
      || ((exact2 #"\"" ++ many quoteParser ++ exact2 #"\"")
          >> (fn ((l,_),(s,_)) => [(l, Tname (implode (map snd s)))]))
      || ((exact2 #"#" ++ many (some2 inComment) ++ exact2 #"\n") >> (K []))
      || (any >> (fn (l,c) => lexError l ("bad char: \"" ^ str c ^ "\"")));

  val tokenParser =
      ((many (some2 Char.isSpace) ++ tokParser) >> snd)
      || ((atLeastOne (some2 Char.isSpace) ++ finished) >> K []);

  (* Interpreting commands *)

  type state =
       {globals : theorems,
        stack : stack,
        dict : dict,
        saved : saved};

  fun execute translation cmd {globals,stack,dict,saved} =
      (case cmd of

       (* Errors *)

         Tcommand "error" =>
         let
           val stack = pushStack (Oerror,Pnull) stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       (* Numbers *)

       | Tnum i =>
         let
           val stack = pushStack (Onum i, Pnull) stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       (* Names *)

       | Tname n =>
         let
           val stack = pushStack (Oname n, Pnull) stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       (* Lists *)

       | Tcommand "nil" =>
         let
           val stack = pushStack (Olist [], Pnull) stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       | Tcommand "cons" =>
         (case stack of
            (objT as Op {object = Olist t, provenance = provT, ...}, _) ::
            (objH as Op {object = h, provenance = provH, ...}, _) ::
            stack =>
            let
              val () =
                  case h of
                    Ocall _ =>
                    raise Error "cons may not operate on Ocall objects"
                  | _ => ()
              val provenance =
                  case (provH,provT) of
                    (Pnull,Pnull) => Pnull
                  | _ => Pcons (objH,objT)
              val stack = pushStack (Olist (h :: t), provenance) stack
            in
              {globals = globals, stack = stack, dict = dict, saved = saved}
            end
          | _ => raise Error "bad arguments")

       (* Types *)

       | Tcommand "type_var" =>
         (case stack of
            (Op {object = Oname n, ...}, _) :: stack =>
            let
              val stack = pushStack (Otype (mkTypeVar n), Pnull) stack
            in
              {globals = globals, stack = stack, dict = dict, saved = saved}
            end
          | _ => raise Error "bad arguments")

       | (Tcommand "type_op", Olist l :: Oname n :: stack) =>
         let
           val n = importType translation n
           val (ty,extra) = extraTypeOp translation (n, map destOtype l) extra
           val stack = Otype ty :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
        end

       (* Terms *)

       | (Tcommand "var", Otype ty :: Oname n :: stack) =>
         let
           val stack = Oterm (mkVar (n,ty)) :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "const", Otype ty :: Oname n :: stack) =>
         let
           val n = importConst translation n
           val (tm,extra) = extraConst translation (n,ty) extra
           val stack = Oterm tm :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "comb", Oterm b :: Oterm a :: stack) =>
         let
           val stack = Oterm (mkComb (a,b)) :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "abs", Oterm b :: Oterm v :: stack) =>
         let
           val stack = Oterm (mkAbs (destVar v, b)) :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       (* Theorems *)

       | (Tcommand "thm", Oterm c :: Olist h :: stack) =>
         let
           val h = map destOterm h
           val (th,extra) = extraThm translation (h,c) extra
           val stack = Othm th :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
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
           {globals = globals, stack = stack, dict = dict, saved = saved}
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
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       (* Dictionary *)

       | (Tcommand "def", Onum n :: d :: stack) =>
         let
           val dict = dictDef (n,d) dict
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "ref", Onum n :: stack) =>
         let
           val obj = dictRef dict n
           val stack = obj :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       (* General *)

       | (Tcommand "pop", _ :: stack) =>
         {globals = globals, stack = stack, dict = dict, saved = saved}

       | (Tcommand "dup", Onum n :: stack) =>
         let
           val _ = length stack > n orelse raise Error "bad dup"
           val stack = List.nth (stack,n) :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "save", Othm th :: stack) =>
         let
           val saved = savedAdd th saved
           val extra = extraSave th extra
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       (* Any other command is an error *)

       | Tcommand _ => raise Error "unknown command")
      handle Error err => raise Error (tokToString cmd ^ ": " ^ err);

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
