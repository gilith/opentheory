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
(* Invariants *in order of priority*                                         *)
(*                                                                           *)
(* 1. The provenance of an Ocall object is the object that became the call   *)
(*    argument.                                                              *)
(*                                                                           *)
(* 2. Objects do not contain theorems iff they have provenance Pnull.        *)
(*    [Objects that do not contain theorems can be easily constructed.]      *)
(*                                                                           *)
(* 3. The provenance of a call argument is the object that became the call   *)
(*    argument.                                                              *)
(*    [Technically redundant, but necessary because of Invariant 2.]         *)
(*                                                                           *)
(* 4. The provenance of a return value is the object that became the return  *)
(*    value.                                                                 *)
(*                                                                           *)
(* 5. If a theorem can be inferred from theorem-containing objects, then the *)
(*    provenance of the theorem is these objects.                            *)
(*    [This will happen most often when simulating an inference rule and     *)
(*    making use of the call argument - resulting in a singleton list.]      *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Object of
      {objectId : objectId,
       object : Object.object,
       provenance : provenance,
       call : object option}

and provenance =
    Pnull
  | Pcall of object
  | Preturn of object
  | Pcons of object * object
  | Pdup of object
  | Pref of object
  | Pthm of object list;

fun object (Object {object = x, ...}) = x;

fun callObject (Object {call = x, ...}) = x;

fun callStackObject obj =
    case callObject obj of
      NONE => []
    | SOME c => c :: callStackObject c;

fun mkObject (object,provenance,call) =
    let
      val objectId = newObjectId ()
    in
      Object
        {objectId = objectId,
         object = object,
         provenance = provenance,
         call = call}
    end;

fun containsThmsObject obj =
    case obj of
      Object {object = Object.Ocall _, ...} => false
    | Object {provenance = Pnull, ...} => false
    | _ => true;

(* ------------------------------------------------------------------------- *)
(* Theorems in scope.                                                        *)
(* ------------------------------------------------------------------------- *)

datatype theorems =
    Theorems of (Thm.thm * object list) SequentMap.map;

val noTheorems = Theorems (SequentMap.new ());

fun sizeTheorems (Theorems thms) = SequentMap.size thms;

fun addTheorems thms th_deps =
    let
      val Theorems thmMap = thms
      and (th,_) = th_deps
    in
      Theorems (SequentMap.insert thmMap (sequent th, th_deps))
    end;

local
  fun add deps (th,thms) = addTheorems thms (th,deps);
in
  fun addObjectTheorems thms obj =
      List.foldl (add [obj]) thms (Object.thms (object obj));
end;

fun searchTheorems (Theorems thmMap) seq =
    case SequentMap.peek thmMap seq of
      NONE => NONE
    | SOME (th,deps) => SOME (alpha seq th, deps);

(* ------------------------------------------------------------------------- *)
(* Stacks.                                                                   *)
(* ------------------------------------------------------------------------- *)

datatype stack =
    Stack of
      {size : int,
       objects : object list,
       theorems : theorems list};

val emptyStack = Stack {size = 0, objects = [], theorems = []};

fun sizeStack (Stack {size = x, ...}) = x;

fun objectStack (Stack {objects = x, ...}) = x;

fun countThmsStack (Stack {theorems,...}) =
    case theorems of
      [] => 0
    | ths :: _ => sizeTheorems ths;

fun pushStack stack obj =
    let
      val Stack {size,objects,theorems} = stack

      val size = size + 1

      val objects = obj :: objects

      val thms =
          case theorems of
            [] => noTheorems
          | thms :: _ => thms

      val thms = addObjectTheorems thms obj

      val theorems = thms :: theorems
    in
      Stack
        {size = size,
         objects = objects,
         theorems = theorems}
    end;

fun popStack stack n =
    let
      val Stack {size,objects,theorems} = stack
    in
      if n > size then raise Error "Article.popStack: empty"
      else
        Stack
          {size = size - n,
           objects = List.drop (objects,n),
           theorems = List.drop (theorems,n)}
    end;

fun peekStack stack n =
    let
      val Stack {size,objects,...} = stack
    in
      if n >= size then raise Error "Article.peekStack: bad index"
      else List.nth (objects,n)
    end;

fun pop1Stack stack =
    (popStack stack 1,
     peekStack stack 0);

fun pop2Stack stack =
    (popStack stack 2,
     peekStack stack 1,
     peekStack stack 0);

local
  fun topCall _ [] = NONE
    | topCall n (Object {object = ob, ...} :: objs) =
      case ob of
        Object.Ocall s => SOME (n,s)
      | _ => topCall (n + 1) objs;
in
  fun popCallStack stack =
      case topCall 1 (objectStack stack) of
        NONE => raise Error "Article.popCallStack: top level"
      | SOME (n,s) => (s, popStack stack n);
end;

fun topStack (Stack {objects,...}) =
    case objects of
      [] => NONE
    | obj :: _ => SOME obj;

fun topCallStack stack =
    case topStack stack of
      NONE => NONE
    | SOME obj =>
      if Object.isOcall (object obj) then SOME obj
      else callObject obj;

fun topCallToStringStack stack =
    case topCallStack stack of
      NONE => "top level"
    | SOME obj =>
      case object obj of
        Object.Ocall n => Name.toString n
      | _ => raise Bug "Article.topCallToStringStack";

fun callStack stack =
    case topCallStack stack of
      NONE => []
    | SOME obj => obj :: callStackObject obj;

fun searchStack (Stack {theorems,...}) seq =
    case theorems of
      [] => NONE
    | thms :: _ => searchTheorems thms seq;

(* ------------------------------------------------------------------------- *)
(* Dictionaries.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype dict = Dict of object IntMap.map;

val emptyDict = Dict (IntMap.new ());

fun defDict (Dict dict) (n,obj) = Dict (IntMap.insert dict (n,obj));

fun refDict (Dict dict) n =
    case IntMap.peek dict n of
      SOME obj => obj
    | NONE => raise Error ("refDict: no entry for key " ^ Int.toString n);

(* ------------------------------------------------------------------------- *)
(* Saved theorems.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype saved = Saved of stack;

val emptySaved = Saved emptyStack;

fun addSaved (Saved stack) obj =
    if Object.isOthm (object obj) then Saved (pushStack stack obj)
    else raise Error "Object.addSaved: not an Othm object";

fun searchSaved (Saved stack) seq =
    case searchStack stack seq of
      SOME (th,_) => SOME th
    | NONE => NONE;

fun listSaved (Saved stack) = rev (objectStack stack);

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
**)

(* ------------------------------------------------------------------------- *)
(* hol-light                                                                 *)
(* ------------------------------------------------------------------------- *)

fun holLightTypeSubstToSubst oins =
    let
      fun f (x,y) = (destTypeVar (Object.destOtype y), Object.destOtype x)
      val l = Object.destOlist oins
    in
      TermSubst.fromListType (map (f o Object.destOpair) l)
    end
    handle Error err =>
      raise Bug ("holLightTypeSubstToSubst failed:\n" ^ err);

fun holLightSubstToSubst oins =
    let
      fun f (x,y) = (destVar (Object.destOterm y), Object.destOterm x)
      val l = Object.destOlist oins
    in
      TermSubst.fromList (map (f o Object.destOpair) l)
    end
    handle Error err =>
      raise Bug ("holLightSubstToSubst failed:\n" ^ err);

fun holLightNewBasicDefinition arg =
    let
      val tm = Object.destOterm arg
      val (v,t) = destEq tm
      val (n,ty) = destVar v
      val v = mkVar (n,ty)
      val tm = mkEq (v,t)
    in
      Object.Othm (define tm)
    end
    handle Error err =>
      raise Bug ("holLightNewBasicDefinition failed:\n" ^ err);

fun holLightNewBasicTypeDefinition arg =
    let
      val (name,absRep,nonEmptyTh) = Object.destOtriple arg
      val name = Object.destOname name
      val (abs,rep) = Object.destOpair absRep
      val abs = Object.destOname abs
      and rep = Object.destOname rep
      and nonEmptyTh = Object.destOthm nonEmptyTh
      val tyVars = NameSet.toList (Term.typeVars (concl nonEmptyTh))
      val (absRepTh,repAbsTh) =
          defineType name {abs = abs, rep = rep} tyVars nonEmptyTh
    in
      Object.mkOpair (Object.Othm absRepTh, Object.Othm repAbsTh)
    end
    handle Error err =>
      raise Bug ("holLightNewBasicTypeDefinition failed:\n" ^ err);

fun holLightAbs arg =
    let
      val (otm,oth) = Object.destOpair arg
      val v = destVar (Object.destOterm otm)
      val th = Object.destOthm oth
    in
      Object.Othm (abs v th)
    end;

fun holLightAssume arg = Object.Othm (assume (Object.destOterm arg));

fun holLightBeta arg = Object.Othm (betaConv (Object.destOterm arg));

fun holLightDeductAntisymRule arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (deductAntisym th1 th2)
    end;

fun holLightEqMp arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (eqMp th1 th2)
    end;

fun holLightInst arg =
    let
      val (oins,oth) = Object.destOpair arg
      val ins = holLightSubstToSubst oins
      val th = Object.destOthm oth
    in
      Object.Othm (subst ins th)
    end;

fun holLightInstType arg =
    let
      val (oins,oth) = Object.destOpair arg
      val ins = holLightTypeSubstToSubst oins
      val th = Object.destOthm oth
    in
      Object.Othm (subst ins th)
    end;

fun holLightMkComb arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (comb th1 th2)
    end;

fun holLightRefl arg = Object.Othm (refl (Object.destOterm arg));

fun holLightTrans arg =
    let
      val (oth1,oth2) = Object.destOpair arg
      val th1 = Object.destOthm oth1
      val th2 = Object.destOthm oth2
    in
      Object.Othm (trans th1 th2)
    end;

val holLightNamespace = Namespace.mkNested (Namespace.global,"hol-light");

val holLightSimulations =
    map (fn (s,f) => (Name.mk (holLightNamespace,s), f))
      [("new_basic_definition", holLightNewBasicDefinition),
       ("new_basic_type_definition", holLightNewBasicTypeDefinition),
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

(* ------------------------------------------------------------------------- *)
(* Simulating other theorem provers.                                         *)
(* ------------------------------------------------------------------------- *)

val simulations = NameMap.fromList holLightSimulations;

fun simulate stack seq =
    case topCallStack stack of
      SOME (Object {object = Object.Ocall f, provenance = Pcall a, ...}) =>
      let
        val Object {object = a, ...} = a
      in
        case NameMap.peek simulations f of
          NONE => NONE
        | SOME sim =>
          let
            val r = sim a
            val ths = Object.thms r
          in
            case first (total (alpha seq)) ths of
              SOME th => SOME (th,[])
            | NONE => NONE
          end
      end
    | _ => NONE;

(* ------------------------------------------------------------------------- *)
(* Commands.                                                                 *)
(* ------------------------------------------------------------------------- *)

datatype command =
    Cnum of int
  | Cname of Name.name
  | Cop of string;

fun ppCommand p cmd =
    case cmd of
      Cnum i => Parser.ppInt p i
    | Cname n => Name.ppQuoted p n
    | Cop c => Parser.ppString p c;

val commandToString = Parser.toString ppCommand;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parser;

  fun isAlphaNum #"_" = true
    | isAlphaNum c = Char.isAlphaNum c;

  val space = many (some Char.isSpace) >> K ();

  val cnumParser =
      atLeastOne (some Char.isDigit) >>
      (Cnum o natFromString "bad number" o implode);

  val cnameParser = Name.quotedParser >> Cname;

  val tcommandParser =
      (some Char.isAlpha ++ many (some isAlphaNum)) >>
      (Cop o implode o op::);
in
  val commandParser = cnumParser || cnameParser || tcommandParser;

  val spacedCommandParser =
      (space ++ commandParser ++ space) >> (fn ((),(t,())) => [t]);
end;

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype state =
    State of
      {stack : stack,
       dict : dict,
       saved : saved};

val initialState =
    State
      {stack = emptyStack,
       dict = emptyDict,
       saved = emptySaved};

fun savedState (State {saved = s, ...}) = s;

fun stackState (State {stack = s, ...}) = s;

fun execute interpretation cmd state =
    let
      val State {stack,dict,saved} = state
    in
      case cmd of
      (* Errors *)

        Cop "error" =>
        let
          val ob = Object.Oerror
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Numbers *)

      | Cnum i =>
        let
          val ob = Object.Onum i
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Names *)

      | Cname n =>
        let
          val ob = Object.Oname n
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Lists *)

      | Cop "nil" =>
        let
          val ob = Object.onil
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "cons" =>
        let
          val (stack,
               objH as Object {object = obH, ...},
               objT as Object {object = obT, ...}) = pop2Stack stack
          val ob = Object.mkOcons (obH,obT)
          and prov =
              if containsThmsObject objH orelse containsThmsObject objT then
                Pcons (objH,objT)
              else
                Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Types *)

      | Cop "type_var" =>
        let
          val (stack,
               Object {object = obN, ...}) = pop1Stack stack
          val ob = Object.mkOtypeVar obN
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "type_op" =>
        let
          val (stack,
               Object {object = obN, ...},
               Object {object = obL, ...}) = pop2Stack stack
          val obN = Object.interpretType interpretation obN
          val ob = Object.mkOtypeOp (obN,obL)
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Terms *)

      | Cop "var" =>
        let
          val (stack,
               Object {object = obN, ...},
               Object {object = obT, ...}) = pop2Stack stack
          val ob = Object.mkOtermVar (obN,obT)
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "const" =>
        let
          val (stack,
               Object {object = obN, ...},
               Object {object = obT, ...}) = pop2Stack stack
          val obN = Object.interpretConst interpretation obN
          val ob = Object.mkOtermConst (obN,obT)
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "comb" =>
        let
          val (stack,
               Object {object = obF, ...},
               Object {object = obA, ...}) = pop2Stack stack
          val ob = Object.mkOtermComb (obF,obA)
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "abs" =>
        let
          val (stack,
               Object {object = obV, ...},
               Object {object = obB, ...}) = pop2Stack stack
          val ob = Object.mkOtermAbs (obV,obB)
          and prov = Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Theorems *)

      | Cop "thm" =>
        let
          val (stack,
               Object {object = obH, ...},
               Object {object = obC, ...}) = pop2Stack stack

          val seq =
              {hyp = TermAlphaSet.fromList (Object.destOterms obH),
               concl = Object.destOterm obC}

          val (th,deps) =
              case searchSaved saved seq of
                SOME th => (th,[])
              | NONE =>
                case simulate stack seq of
                  SOME th_deps => th_deps
                | NONE =>
                  case searchStack stack seq of
                    SOME th_deps => th_deps
                  | NONE =>
                    let
                      val th = Thm.axiom seq
                      val () = warn ("making new axiom in " ^
                                     topCallToStringStack stack ^ ":\n" ^
                                     thmToString th)
                    in
                      (th,[])
                    end

          val ob = Object.Othm th
          and prov = Pthm deps
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Function calls *)

      | Cop "call" =>
        let
          val (stack,
               objA as Object {object = obA, ...},
               Object {object = obN, ...}) = pop2Stack stack
          val _ = not (Object.isOcall obA) orelse
                  raise Error "cannot use an Ocall object as a call argument"
          val n = Object.destOname obN
          val n = Interpretation.interpretRule interpretation n
(*OpenTheoryTrace1
          val () = if not (null (callStack stack)) then ()
                   else trace (Name.toString n ^ "\n")
*)
(*OpenTheoryTrace2
          val () = trace ("  stack = ["^Int.toString (sizeStack stack) ^
                          "], call stack = [" ^
                          Int.toString (length (callStack stack))^"]\n")
          val () = Parser.ppTrace Object.pp "  input" obA
*)
          val ob = Object.Ocall n
          and prov = Pcall objA
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
          val ob = obA
          and prov = if containsThmsObject objA then Pcall objA else Pnull
          and call = SOME obj
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "return" =>
        let
          val (stack,
               objR as Object {object = obR, ...},
               Object {object = obN, ...}) = pop2Stack stack
          val _ = not (Object.isOcall obR) orelse
                  raise Error "cannot use an Ocall object as a return value"
          val n = Object.destOname obN
          val n = Interpretation.interpretRule interpretation n
          val (n',stack) = popCallStack stack
          val _ = Name.equal n' n orelse
                  raise Error ("call " ^ Name.toString n' ^
                               " matched by return " ^ Name.toString n)
(*OpenTheoryTrace1
          val () = if not (null (callStack stack)) then ()
                   else trace (Name.toString n ^ " return\n")
*)
(*OpenTheoryTrace2
          val () = trace ("  stack = ["^Int.toString (sizeStack stack) ^
                          "], call stack = [" ^
                          Int.toString (length (callStack stack))^"]\n")
          val () = Parser.ppTrace Object.pp "return" obR
*)
          val ob = obR
          and prov = if containsThmsObject objR then Preturn objR else Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Dictionary *)

      | Cop "def" =>
        let
          val (stack,
               objD as Object {object = obD, ...},
               Object {object = obI, ...}) = pop2Stack stack
          val _ = not (Object.isOcall obD) orelse
                  raise Error "cannot def an Ocall object"
          val i = Object.destOnum obI
          val dict = defDict dict (i,objD)
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "ref" =>
        let
          val (stack,
               Object {object = obI, ...}) = pop1Stack stack
          val i = Object.destOnum obI
          val objD as Object {object = obD, ...} = refDict dict i
          val ob = obD
          and prov = if containsThmsObject objD then Pref objD else Pnull
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* General *)

      | Cop "pop" =>
        let
          val stack = popStack stack 1
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "dup" =>
        let
          val (stack,
               Object {object = obI, ...}) = pop1Stack stack
          val i = Object.destOnum obI
          val objD = peekStack stack i
          val ob = object objD
          and prov = if containsThmsObject objD then Pdup objD else Pnull
          and call = topCallStack stack
          val _ = not (Object.isOcall ob) orelse
                  raise Error "cannot dup an Ocall object"
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "save" =>
        let
          val (stack,
               objT) = pop1Stack stack
          val saved = addSaved saved objT
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Any other command is an error *)

      | Cop _ => raise Error "unknown command"
    end
    handle Error err => raise Error (commandToString cmd ^ ": " ^ err);

(* ------------------------------------------------------------------------- *)
(* Articles                                                                  *)
(* ------------------------------------------------------------------------- *)

datatype article =
    Article of
      {saved : saved};

fun search (Article {saved,...}) seq = searchSaved saved seq;

fun saved (Article {saved = s, ...}) =
    map (Object.destOthm o object) (listSaved s);

(***
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
(*OpenTheoryTrace1
      val _ = trace ("deadTheoremElimination: before = " ^
                     articleToString article ^ "\n")
*)
      val Article ops = article
      val (ops,useful) = eliml ops useful
      val article = Article ops
(*OpenTheoryTrace1
      val _ = trace ("deadTheoremElimination: after = " ^
                     articleToString article ^ "\n")
*)
    in
      article
    end;
end;
***)

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
                   newTypes : NameSet.set,
                   newConsts : NameSet.set,
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
      val newTypes = NameSet.add newTypes n
    in
      extraUpdateNewTypes newTypes extra
    end;

fun extraAddNewConst n extra =
    let
      val newConsts = extraNewConsts extra
      val newConsts = NameSet.add newConsts n
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
       newTypes = NameSet.empty,
       newConsts = NameSet.empty,
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
  val constExists = can Term.constType;

  fun extraNewConst n extra =
      let
        val () = warn ("making new constant " ^ n)
        val () = Term.declareConst n alpha
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
      val saved = listSaved saved
      val useful = thmDepsUseful (extraThmDeps extra) saved
      val article = deadTheoremElimination useful article
    in
      (saved,article)
    end;
***)

(* ------------------------------------------------------------------------- *)
(* I/O                                                                       *)
(* ------------------------------------------------------------------------- *)

(***
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
           State {stack = stack, dict = dict, saved = saved}
         end

       (* Numbers *)

       | Tnum i =>
         let
           val stack = pushStack (Onum i, Pnull) stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       (* Names *)

       | Tname n =>
         let
           val stack = pushStack (Oname n, Pnull) stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       (* Lists *)

       | Tcommand "nil" =>
         let
           val stack = pushStack (Olist [], Pnull) stack
         in
           State {stack = stack, dict = dict, saved = saved}
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
              State {stack = stack, dict = dict, saved = saved}
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
              State {stack = stack, dict = dict, saved = saved}
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
              State {stack = stack, dict = dict, saved = saved}
            end
          | _ => raise Error "bad arguments")

       (* Terms *)

       | Tcommand "var", Otype ty :: Oname n :: stack) =>
         let
           val stack = Oterm (mkVar (n,ty)) :: stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "const", Otype ty :: Oname n :: stack) =>
         let
           val n = exportConst translation n
           val (tm,extra) = extraConst translation (n,ty) extra
           val stack = Oterm tm :: stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "comb", Oterm b :: Oterm a :: stack) =>
         let
           val stack = Oterm (mkComb (a,b)) :: stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "abs", Oterm b :: Oterm v :: stack) =>
         let
           val stack = Oterm (mkAbs (destVar v, b)) :: stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       (* Theorems *)

       | (Tcommand "thm", Oterm c :: Olist h :: stack) =>
         let
           val h = map destOterm h
           val (th,extra) = extraThm translation (h,c) extra
           val stack = Othm th :: stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       (* Function calls *)

       | (Tcommand "call", Oname n :: i :: stack) =>
         let
           val n = exportRule translation n
(*OpenTheoryTrace1
           val () = if not (null (callStack stack)) then ()
                    else trace (n ^ "\n")
*)
(*OpenTheoryTrace2
           val () = trace ("  stack = ["^Int.toString (length stack) ^
                           "], call stack = [" ^
                           Int.toString (length (callStack stack))^"]\n")
           val () = Parser.ppTrace "  input" ppObject i
*)
           val stack = i :: Ocall n :: stack
           val extra = extraCall n i extra
         in
           State {stack = stack, dict = dict, saved = saved}
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
(*OpenTheoryTrace1
           val () = if not (null (callStack stack)) then ()
                    else trace (n ^ " return\n")
*)
(*OpenTheoryTrace2
           val () = trace ("  stack = ["^Int.toString (length stack) ^
                           "], call stack = [" ^
                           Int.toString (length (callStack stack)) ^ "]\n")
           val () = Parser.ppTrace "return" ppObject r
*)
           val extra = extraReturn n r extra
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       (* Dictionary *)

       | (Tcommand "def", Onum n :: d :: stack) =>
         let
           val dict = defDict (n,d) dict
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "ref", Onum n :: stack) =>
         let
           val obj = refDict dict n
           val stack = obj :: stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       (* General *)

       | (Tcommand "pop", _ :: stack) =>
         State {stack = stack, dict = dict, saved = saved}

       | (Tcommand "dup", Onum n :: stack) =>
         let
           val _ = length stack > n orelse raise Error "bad dup"
           val stack = List.nth (stack,n) :: stack
         in
           State {stack = stack, dict = dict, saved = saved}
         end

       | (Tcommand "save", Othm th :: stack) =>
         let
           val saved = addSaved th saved
           val extra = extraSave th extra
         in
           State {stack = stack, dict = dict, saved = saved}
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
(*OpenTheoryTrace1
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
        val (saved,extra) = foldl f (emptySaved,extraInit) l
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
***)

fun fromTextFile {filename,interpretation} =
    let
      (* Comment lines *)

      fun isComment l =
          case List.find (not o Char.isSpace) l of
            NONE => true
          | SOME #"#" => true
          | _ => false

      (* Estimating parse error line numbers *)

      val lines = Stream.fromTextFile {filename = filename}

      (* The character stream *)

      val {chars,parseErrorLocation} = Parser.initialize {lines = lines}

      val chars = Stream.filter (not o isComment) chars

      val chars = Parser.everything Parser.any chars

      (* The command stream *)

      val commands =
          Parser.everything spacedCommandParser chars
          handle Parser.NoParse =>
            raise Error ("parse error in file \"" ^ filename ^ "\" " ^
                         parseErrorLocation ())

      (* Executing the commands *)

      fun process (command,state) =
          execute interpretation command state
          handle Error err =>
            raise Error ("error in file \"" ^ filename ^ "\" " ^
                         parseErrorLocation () ^ "\n" ^ err)

      val state = initialState

      val state = Stream.foldl process state commands

      val () =
          let
            val stack = stackState state
            val n = countThmsStack stack
          in
            if n = 0 then ()
            else
              warn (Int.toString n ^ " theorem" ^ (if n = 1 then "" else "s") ^
                    " left on the stack by " ^ filename)
          end
    in
      Article {saved = savedState state}
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
