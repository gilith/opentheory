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
(* 3. The provenance of a return value is the object that became the return  *)
(*    value.                                                                 *)
(*                                                                           *)
(* 4. If a theorem can be inferred from theorem-containing objects, then the *)
(*    provenance of the theorem is these objects.                            *)
(*    [This will happen most often when simulating an inference rule and     *)
(*    making use of the call argument - resulting in a singleton list.]      *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Object of
      {id : objectId,
       object : Object.object,
       provenance : provenance,
       call : object option}

and provenance =
    Pnull
  | Pcall of object
  | Preturn of object
  | Pcons of object * object
  | Pref of object
  | Pthm of object list;

fun compareObject (Object {id = i1, ...}, Object {id = i2, ...}) =
    Int.compare (i1,i2);

fun object (Object {object = x, ...}) = x;

fun callObject (Object {call = x, ...}) = x;

(***
fun objectsProvenance prov =
    case prov of
      Pnull => []
    | Pcall obj => [obj]
    | Preturn obj => [obj]
    | Pcons (obj1,obj2) => [obj1,obj2]
    | Pdup obj => [obj]
    | Pref obj => [obj]
    | Pthm objs => objs;

fun dependenciesObject (Object {provenance = p, ...}) = objectsProvenance p;
***)

local
  fun maps lr preF postF obj acc =
      case preF obj acc of
        Left obj => (obj,acc)
      | Right obj =>
        let
          val Object {id, object = ob, provenance = prov, call} = obj

          val (prov',call',acc) =
              if lr then
                let
                  val (prov',acc) = mapsProv lr preF postF prov acc
                  val (call',acc) =
                      Sharing.mapsOption (maps lr preF postF) call acc
                in
                  (prov',call',acc)
                end
              else
                let
                  val (call',acc) =
                      Sharing.mapsOption (maps lr preF postF) call acc
                  val (prov',acc) = mapsProv lr preF postF prov acc
                in
                  (prov',call',acc)
                end

          val obj =
              if Portable.pointerEqual (prov',prov) andalso
                 Portable.pointerEqual (call',call) then
                obj
              else
                Object {id = id, object = ob, provenance = prov', call = call'}

          val acc = postF obj acc
        in
          (obj,acc)
        end

  and mapsProv lr preF postF prov acc =
      case prov of
        Pnull => (prov,acc)
      | Pcall obj => mapsProv1 lr preF postF prov acc Pcall obj
      | Preturn obj => mapsProv1 lr preF postF prov acc Preturn obj
      | Pcons (objH,objT) =>
        let
          val (objH',objT',acc) =
              if lr then
                let
                  val (objH',acc) = maps lr preF postF objH acc
                  val (objT',acc) = maps lr preF postF objT acc
                in
                  (objH',objT',acc)
                end
              else
                let
                  val (objT',acc) = maps lr preF postF objT acc
                  val (objH',acc) = maps lr preF postF objH acc
                in
                  (objH',objT',acc)
                end

          val prov' =
              if Portable.pointerEqual (objH',objH) andalso
                 Portable.pointerEqual (objT',objT) then
                prov
              else
                Pcons (objH',objT')
        in
          (prov',acc)
        end
      | Pref obj => mapsProv1 lr preF postF prov acc Pref obj
      | Pthm objs =>
        let
          val (objs',acc) =
              (if lr then Sharing.maps else Sharing.revMaps)
                (maps lr preF postF) objs acc

          val prov' =
              if Portable.pointerEqual (objs',objs) then prov else Pthm objs'
        in
          (prov',acc)
        end

  and mapsProv1 lr preF postF prov acc con obj =
      let
        val (obj',acc) = maps lr preF postF obj acc
        val prov' = if Portable.pointerEqual (obj',obj) then prov else con obj'
      in
        (prov',acc)
      end;
in
  fun mapsObject preF postF obj acc = maps true preF postF obj acc;

  fun revMapsObject preF postF obj acc = maps false preF postF obj acc;
end;

fun callStackObject obj =
    case callObject obj of
      NONE => []
    | SOME c => c :: callStackObject c;

fun mkObject (object,provenance,call) =
    let
      val id = newObjectId ()
    in
      Object
        {id = id,
         object = object,
         provenance = provenance,
         call = call}
    end;

fun containsThmsObject obj =
    case obj of
      Object {object = Object.Ocall _, ...} =>
      raise Bug "Article.containsThmsObject: Ocall"
    | Object {provenance = Pnull, ...} => false
    | _ => true;

(* ------------------------------------------------------------------------- *)
(* Object sets.                                                              *)
(* ------------------------------------------------------------------------- *)

datatype objectSet = ObjectSet of object Set.set;

val emptyObjectSet = ObjectSet (Set.empty compareObject);

fun nullObjectSet (ObjectSet set) = Set.null set;

fun memberObjectSet obj (ObjectSet set) = Set.member obj set;

fun peekObjectSet (ObjectSet set) obj = Set.peek set obj;

fun addObjectSet (ObjectSet set) obj = ObjectSet (Set.add set obj);

fun addListObjectSet set objs =
    List.foldl (fn (obj,acc) => addObjectSet acc obj) set objs;

fun fromListObjectSet objs = addListObjectSet emptyObjectSet objs;

fun foldlObjectSet f b (ObjectSet set) = Set.foldl f b set;

local
  fun toStream NONE = Stream.NIL
    | toStream (SOME iter) =
      Stream.CONS (Set.readIterator iter, toStreamDelay iter)

  and toStreamDelay iter () = toStream (Set.advanceIterator iter)
in
  fun toStreamObjectSet (ObjectSet set) = toStream (Set.mkIterator set);
end;

(***
local
  fun parents (Object {provenance = prov, call, ...}) =
      let
        val pars = []
        val pars = case prov of Pcall obj => obj :: pars | _ => pars
        val pars = case call of SOME obj => obj :: pars | NONE => pars
      in
        pars
      end;

  fun add set [] = set
    | add set (obj :: objs) =
      if Set.member obj set then add set objs
      else add (addObjectSet set obj) (parentsObject obj @ objs);
in
  fun addCallStackObjectSet set obj = ObjectSet (add set [obj]);
end;

val closeCallStackObjectSet =
    let
      fun add (obj,set) = addCallStackObjectSet set obj
    in
      foldlObjectSet add emptyObjectSet
    end;
***)

(* ------------------------------------------------------------------------- *)
(* Reducing objects.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun preReduce obj (reqd_refs as (reqd,refs)) =
      case peekObjectSet reqd obj of
        SOME obj => Left obj
      | NONE =>
        Right
        let
          val Object {id, object = ob, provenance = prov, call} = obj

          fun better rid =
              rid < id andalso
              case prov of
                Preturn (Object {id = rid', ...}) => rid < rid'
              | Pref (Object {id = rid', ...}) => rid < rid'
              | _ => true
        in
          case prov of
            Pnull => obj
          | Pcall _ => obj
          | _ =>
            case ObjectMap.peek refs ob of
              NONE => obj
            | SOME (robj as Object {id = rid, ...}) =>
              if not (better rid) then obj
              else
                Object
                  {id = id,
                   object = ob,
                   provenance = Pref robj,
                   call = call}
        end;

  fun postReduce (obj as Object {id, object = ob, ...}) (reqd,refs) =
      let
        val reqd = addObjectSet reqd obj

        val insertRefs =
            case ObjectMap.peek refs ob of
              NONE => true
            | SOME (Object {id = id', ...}) => id < id'

        val refs = if insertRefs then ObjectMap.insert refs (ob,obj) else refs
      in
        (reqd,refs)
      end;

  fun dependencies clean lr objs =
      let
        val reqd = emptyObjectSet
        val refs = ObjectMap.new ()
        val (objs',(reqd,_)) =
            if lr then
              Sharing.maps (mapsObject preReduce postReduce)
                objs (reqd,refs)
            else
              Sharing.revMaps (revMapsObject preReduce postReduce)
                objs (reqd,refs)

        val clean' = Portable.pointerEqual (objs',objs)

        val lr = not lr
      in
        if clean andalso clean' then (reqd,objs)
        else dependencies clean' lr objs'
      end;
in
  fun dependenciesObject objs =
      dependencies false true objs
      handle Error err =>
        raise Bug ("Article.dependenciesObjects: " ^ err);
end;

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
(* Dictionaries.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype dict = Dict of object IntMap.map;

val emptyDict = Dict (IntMap.new ());

fun defDict (Dict dict) (key,obj) = Dict (IntMap.insert dict (key,obj));

fun refDict (Dict dict) key =
    case IntMap.peek dict key of
      SOME obj => obj
    | NONE => raise Error ("refDict: no entry for key " ^ Int.toString key);

fun removeDict (Dict dict) key =
    case IntMap.peek dict key of
      SOME obj => (Dict (IntMap.delete dict key), obj)
    | NONE => raise Error ("removeDict: no entry for key " ^ Int.toString key);

(* ------------------------------------------------------------------------- *)
(* Stacks.                                                                   *)
(* ------------------------------------------------------------------------- *)

datatype stack =
    Stack of
      {size : int,
       objects : object list,
       theorems : theorems list,
       call : (object * stack) option};

val emptyStack =
    Stack
      {size = 0,
       objects = [],
       theorems = [],
       call = NONE};

fun sizeStack (Stack {size = x, ...}) = x;

fun frameSizeStack (Stack {size,call,...}) =
    size - (case call of NONE => 0 | SOME (_,stack) => sizeStack stack + 1);

fun objectStack (Stack {objects = x, ...}) = x;

fun theoremsStack (Stack {theorems,...}) =
    case theorems of
      [] => noTheorems
    | ths :: _ => ths;

fun pushStack stack obj =
    let
      val Stack {size,objects,theorems,call} = stack

      val size = size + 1

      val objects = obj :: objects

      val thms = addObjectTheorems (theoremsStack stack) obj

      val theorems = thms :: theorems

      val call = if Object.isOcall (object obj) then SOME (obj,stack) else call
    in
      Stack
        {size = size,
         objects = objects,
         theorems = theorems,
         call = call}
    end;

fun popStack stack n =
    if n > frameSizeStack stack then raise Error "Article.popStack: empty frame"
    else
      let
        val Stack {size,objects,theorems,call} = stack

        val size = size - n

        val objects = List.drop (objects,n)

        val theorems = List.drop (theorems,n)
      in
        Stack
          {size = size,
           objects = objects,
           theorems = theorems,
           call = call}
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

fun popCallStack (Stack {call,...}) =
    case call of
      NONE => raise Error "Article.popCallStack: top level"
    | SOME (Object {object = ob, ...}, stack) => (Object.destOcall ob, stack);

fun topCallStack (Stack {call,...}) =
    case call of
      NONE => NONE
    | SOME (obj,_) => SOME obj;

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
          val stack = pushStack stack objA
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
               Object {object = obI, ...}) = pop1Stack stack
          val objD as Object {object = obD, ...} = peekStack stack 0
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

      | Cop "remove" =>
        let
          val (stack,
               Object {object = obI, ...}) = pop1Stack stack
          val i = Object.destOnum obI
          val (dict, objD as Object {object = obD, ...}) = removeDict dict i
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
          and prov = if containsThmsObject objD then Pref objD else Pnull
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
(* Generating simple objects.                                                *)
(* ------------------------------------------------------------------------- *)

fun generateObject ob =
    case ob of
      Object.Oerror => (Cop "error", [])
    | Object.Onum i => (Cnum i, [])
    | Object.Oname n => (Cname n, [])
    | Object.Olist l =>
      (case l of
         [] => (Cop "nil", [])
       | h :: t => (Cop "cons", [h, Object.Olist t]))
    | Object.Otype ty =>
      (case Type.dest ty of
         Type.TypeVar n => (Cop "type_var", [Object.Oname n])
       | Type.TypeOp (n,tys) =>
         (Cop "type_op", [Object.Oname n, Object.mkOtypes tys]))
    | Object.Oterm tm =>
      (case Term.dest tm of
         Term.Const (n,ty) =>
         (Cop "const", [Object.Oname n, Object.Otype ty])
       | Term.Var (n,ty) =>
         (Cop "var", [Object.Oname n, Object.Otype ty])
       | Term.Comb (f,a) =>
         (Cop "comb", [Object.Oterm f, Object.Oterm a])
       | Term.Abs (v,b) =>
         (Cop "abs", [Object.Oterm (Term.mkVar v), Object.Oterm b]))
    | Object.Othm th =>
      let
        val {hyp,concl} = sequent th
        val hyp = TermAlphaSet.toList hyp
      in
        (Cop "thm", [Object.mkOterms hyp, Object.Oterm concl])
      end
    | Object.Ocall _ => raise Bug "Article.generateObject: Ocall";

(* ------------------------------------------------------------------------- *)
(* Generating commands.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype minDict =
    MinDict of
      {nextKey : int,
       refs : int ObjectMap.map,
       keys : int ObjectMap.map};

local
  fun storable ob =
      case ob of
        Object.Oerror => false
      | Object.Onum _ => false
      | Object.Oname _ => false
      | Object.Olist l => not (null l)
      | Object.Otype _ => true
      | Object.Oterm _ => true
      | Object.Othm _ => true
      | Object.Ocall _ => raise Bug "Article.storable: Ocall";

  fun registerTop refs ob =
      let
        val p = ObjectMap.peek refs ob
        val known = Option.isSome p
        val k = Option.getOpt (p,0)
        val refs = ObjectMap.insert refs (ob, k + 1)
      in
        (known,refs)
      end;

  fun registerDeep refs [] = refs
    | registerDeep refs (ob :: obs) =
      if not (storable ob) then registerDeep refs obs
      else
        let
          val (known,refs) = registerTop refs ob
          val obs = if known then obs else snd (generateObject ob) @ obs
        in
          registerDeep refs obs
        end;

  fun register (obj,refs) =
      let
        val Object {object = ob, provenance = prov, ...} = obj
      in
        case prov of
          Pnull => registerDeep refs [ob]
        | Pcall _ => refs
        | Preturn _ => refs
        | Pcons _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ = not known orelse raise Bug "Article.register: Pcons"
*)
          in
            refs
          end
        | Pref _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ = known orelse raise Bug "Article.register: Pref"
*)
          in
            refs
          end
        | Pthm _ => registerDeep refs [ob]
      end;
in
  fun newMinDict objs =
      let
        val nextKey = 1
        val refs = ObjectMap.new ()
        val refs = foldlObjectSet register refs objs
        val refs = ObjectMap.filter (fn (_,n) => n >= 2) refs
        val keys = ObjectMap.new ()
      in
        MinDict
          {nextKey = nextKey,
           refs = refs,
           keys = keys}
      end
      handle Error err =>
        raise Bug ("Article.newMinDict: " ^ err);
end;

local
  fun generate (ob,(dict,cmds)) =
      let
        val MinDict {nextKey,refs,keys} = dict
      in
        case ObjectMap.peek keys ob of
          SOME key =>
          let
            val cmds = Cnum key :: cmds
          in
            case ObjectMap.peek refs ob of
              NONE => raise Bug "generate"
            | SOME n =>
              if n = 1 then
                let
                  val refs = ObjectMap.delete refs ob
                  val keys = ObjectMap.delete keys ob
                  val dict =
                      MinDict {nextKey = nextKey, refs = refs, keys = keys}
                  val cmds = Cop "remove" :: cmds
                in
                  (dict,cmds)
                end
              else
                let
                  val refs = ObjectMap.insert refs (ob, n - 1)
                  val dict =
                      MinDict {nextKey = nextKey, refs = refs, keys = keys}
                  val cmds = Cop "ref" :: cmds
                in
                  (dict,cmds)
                end
          end
        | NONE =>
          let
            val (cmd,pars) = generateObject ob
            val (dict,cmds) = foldl generate (dict,cmds) pars
            val cmds = cmd :: cmds
          in
            case ObjectMap.peek refs ob of
              NONE => (dict,cmds)
            | SOME n =>
              let
                val MinDict {nextKey,refs,keys} = dict
                val key = nextKey
                val nextKey = nextKey + 1
                val keys = ObjectMap.insert keys (ob,key)
                val refs = ObjectMap.insert refs (ob, n - 1)
                val dict =
                    MinDict {nextKey = nextKey, refs = refs, keys = keys}
                val cmds = [Cop "def", Cnum key] @ cmds
              in
                (dict,cmds)
              end
          end
    end;
in
  fun generateMinDict dict obj =
      let
        val Object {object = ob, provenance = prov, ...} = obj
      in
        case prov of
          Pnull => generate (ob,(dict,[]))
        | Pcall _ => (dict, [Cop "call"])
        | Preturn _ => (dict, [Cop "return"])
        | Pcons _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ = not known orelse raise Bug "Article.register: Pcons"
*)
          in
            refs
          end
        | Pref _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ = known orelse raise Bug "Article.register: Pref"
*)
          in
            refs
          end
        | Pthm _ => generate (ob,(dict,[]))
      end
      handle Error err =>
        raise Bug ("Article.generateMinDict: " ^ err);
end;

fun generate saved objs =
    let
      fun gen obj dict =
          let
            val (dict,cmds) = generateMinDict dict obj

            val cmds =
                if not (memberObjectSet obj saved) then cmds
                else Cop "save" :: Cop "dup" :: Cnum 0 :: cmds

            val cmds = rev cmds
          in
            (cmds,dict)
          end

      val dict = newMinDict objs

      val strm = toStreamObjectSet objs
      val strm = Stream.maps gen dict strm
    in
      Stream.flatten (Stream.map Stream.fromList strm)
    end
    handle Error err =>
      raise Bug ("Article.generate: " ^ err);

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
            val n = sizeTheorems (theoremsStack stack)
          in
            if n = 0 then ()
            else
              warn (Int.toString n ^ " theorem" ^ (if n = 1 then "" else "s") ^
                    " left on the stack by " ^ filename)
          end
    in
      Article {saved = savedState state}
    end
    handle Error err => raise Error ("Article.fromTextFile: " ^ err);

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

fun toTextFile {filename,article} =
    let
      val Article {saved,...} = article
      val saved = listSaved saved
      val (objs,saved) = dependenciesObject saved
      val saved = fromListObjectSet saved
      val commands = generate saved objs
      val lines = Stream.map (fn c => commandToString c ^ "\n") commands
    in
      Stream.toTextFile {filename = filename} lines
    end
    handle Error err => raise Error ("Article.toTextFile: " ^ err);

end
