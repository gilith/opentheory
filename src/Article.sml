(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful Syntax Rule;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun plural 1 s = "1 " ^ s
  | plural n s = Int.toString n ^ " " ^ s ^ "s";

fun natFromString err s =
    case Int.fromString s of
      SOME i => i
    | NONE => raise Error err;

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
(* 1. The provenance of an Ocall object is the containing Ocall object (if   *)
(*    there is one), plus the object that became the call argument.          *)
(*                                                                           *)
(* 2. The provenance of a call argument is the Ocall object.                 *)
(*                                                                           *)
(* 3. The provenance of a return value is the Ocall object, plus the object  *)
(*    that became the return value.                                          *)
(*                                                                           *)
(* 4. Objects that do not contain theorems have provenance Pnull             *)
(*    (because they can be easily constructed).                              *)
(*                                                                           *)
(* 5. If a theorem can be inferred from theorem-containing objects on the    *)
(*    stack, then the provenance of the theorem is these objects             *)
(*    (this will happen most often when simulating an inference rule and     *)
(*    making use of the call argument - resulting in a singleton list).      *)
(*                                                                           *)
(* 6. If an object is retrieved from the dictionary, then the provenance is  *)
(*    the object that was put into the dictionary.                           *)
(* ------------------------------------------------------------------------- *)

datatype objectProvenance =
    Op of
      {objectId : objectId,
       object : object,
       provenance : provenance}

and provenance =
    Pcall of
      {containingCall : objectProvenance option,
       argument : objectProvenance}
  | PcallArgument of
      {call : objectProvenance}
  | PreturnValue of
      {call : objectProvenance,
       return : objectProvenance}
  | Pnull
  | Pcons of objectProvenance * objectProvenance
  | PinferThm of objectProvenance list
  | Pref of objectProvenance;

fun object (Op {object = x, ...}) = x;

fun destOpthm obj = destOthm (object obj);

fun destOpcall obj = destOcall (object obj);

fun opThms obj = objectThms (object obj);

(* ------------------------------------------------------------------------- *)
(* Theorems in scope.                                                        *)
(* ------------------------------------------------------------------------- *)

datatype theorems =
    Theorems of (Thm.thm * objectProvenance) SequentMap.map;

val noTheorems = Theorems (SequentMap.new ());

fun addTheorems thms th_obj =
    let
      val Theorems thmMap = thms
      and (th,obj) = th_obj
    in
      Theorems (SequentMap.insert thmsMap (sequent th, th_obj))
    end;

local
  fun add obj (th,thms) = addTheorems thms (th,obj);
in
  fun addOpTheorems thms obj = List.foldl (add obj) thms (opThms obj);
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

(*
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
*)

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

(***
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
***)

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
      val n = exportConst holLight n
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
      val name = exportType holLight (destOname name)
      val (abs,rep) = destOpair absRep
      val abs = exportConst holLight (destOname abs)
      and rep = exportConst holLight (destOname rep)
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
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||
         
  open Parser

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

  datatype token = Tnum of int | Tname of string | Tcommand of string;

  fun tokenToString token =
      case token of
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

  local
    fun inComment c = c <> #"\n";

    val space = many (some2 Char.isSpace) >> K ();

    val tnumToken =
        atLeastOne (some2 Char.isDigit) >>
        (singleton o (map2 (Tnum o natFromString "bad number")) o implode2);

    val tnameToken =
        (exact2 #"\"" ++ many quoteParser ++ exact2 #"\"") >>
        (fn ((l,_),(s,_)) => [(l, Tname (implode (map snd s)))]);

    val tcommandToken =
        (some2 Char.isAlpha ++ many (some2 isAlphaNum)) >>
        (singleton o map2 Tcommand o implode2 o op::);

    val commentLine =
        (exact2 #"#" ++ many (some2 inComment) ++ exact2 #"\n") >> K [];

    val errorChar =
        any >> (fn (l,c) => lexError l ("bad char: \"" ^ str c ^ "\""));

    val tokParser =
        tnumToken || tnameToken || tcommandToken || commentLine || errorChar;
  in
    val tokenParser = (space ++ tokParser ++ space) >> (fn ((),(t,())) => t);
  end;

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
                    Ocall _ => raise Error "cannot cons Ocall objects"
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
            (Op {object = Oname n, ...}, _) ::
            stack =>
            let
              val stack = pushStack (Otype (mkTypeVar n), Pnull) stack
            in
              {globals = globals, stack = stack, dict = dict, saved = saved}
            end
          | _ => raise Error "bad arguments")

       | Tcommand "type_op" =>
         (case stack of
            (Op {object = Olist l, ...}, _) ::
            (Op {object = Oname n, ...}, _) ::
            stack =>
            let
              val n = exportType translation n
              and l = map destOtype l
              val () =
                  if can Type.typeArity n then ()
                  else Type.declareType n (length l)
              val stack = pushStack (Otype (mkTypeOp (n,l)), Pnull) stack
            in
              {globals = globals, stack = stack, dict = dict, saved = saved}
            end
          | _ => raise Error "bad arguments")

       (* Terms *)

       | Tcommand "var", Otype ty :: Oname n :: stack) =>
         let
           val stack = Oterm (mkVar (n,ty)) :: stack
         in
           {globals = globals, stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "const", Otype ty :: Oname n :: stack) =>
         let
           val n = exportConst translation n
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
           val n = exportRule translation n
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
           val n = exportRule translation n
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
      handle Error err => raise Error (tokenToString cmd ^ ": " ^ err);

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
        val tokens = everything tokenParser chars
        val {stack,saved,extra,...} = interpret translation saved extra tokens
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
